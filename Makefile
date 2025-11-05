# Makefile for modular fuss
FC = gfortran
FFLAGS = -O2 -Wall -ffree-line-length-none
SRC_DIR = src
BUILD_DIR = build
BIN_DIR = .
VERSION := $(shell cat VERSION 2>/dev/null || echo "unknown")

# Module files (order matters for dependencies)
MODULES = version_module.f90 types_module.f90 cache_module.f90 terminal_module.f90 git_module.f90 tree_module.f90 display_module.f90
MODULE_OBJS = $(MODULES:%.f90=$(BUILD_DIR)/%.o)

# Main program
MAIN = fuss_main.f90
MAIN_OBJ = $(BUILD_DIR)/fuss_main.o

# Target executable
TARGET = $(BIN_DIR)/fuss

# Disable parallel builds to ensure proper module compilation order
.NOTPARALLEL:

.PHONY: all clean

all: $(TARGET)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Generate version module before building
$(SRC_DIR)/version_module.f90: VERSION
	@echo "Generating version module..."
	@echo "module version_module" > $@
	@echo "    implicit none" >> $@
	@echo "    character(len=*), parameter :: VERSION = '$(VERSION)'" >> $@
	@echo "end module version_module" >> $@

# Build modules with explicit dependencies
$(BUILD_DIR)/version_module.o: $(SRC_DIR)/version_module.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/types_module.o: $(SRC_DIR)/types_module.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/cache_module.o: $(SRC_DIR)/cache_module.f90 $(BUILD_DIR)/types_module.o | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/terminal_module.o: $(SRC_DIR)/terminal_module.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/git_module.o: $(SRC_DIR)/git_module.f90 $(BUILD_DIR)/types_module.o $(BUILD_DIR)/cache_module.o | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/tree_module.o: $(SRC_DIR)/tree_module.f90 $(BUILD_DIR)/types_module.o | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/display_module.o: $(SRC_DIR)/display_module.f90 $(BUILD_DIR)/tree_module.o $(BUILD_DIR)/types_module.o | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

# Build main program
$(MAIN_OBJ): $(SRC_DIR)/$(MAIN) $(MODULE_OBJS) | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

# Link everything
$(TARGET): $(MODULE_OBJS) $(MAIN_OBJ)
	$(FC) $(FFLAGS) -o $@ $^

clean:
	rm -rf $(BUILD_DIR) $(TARGET) $(SRC_DIR)/version_module.f90
