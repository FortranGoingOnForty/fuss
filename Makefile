# Makefile for FUSS - Fortran Utility for Showing Status

# Compiler and flags
FC = gfortran
FFLAGS = -O2 -Wall -std=f2008
DEBUGFLAGS = -g -O0 -Wall -std=f2008 -fbacktrace -fcheck=all

# Target executable
TARGET = fuss

# Source files
SOURCES = fuss.f90

# Object files
OBJECTS = $(SOURCES:.f90=.o)

# Default target
all: $(TARGET)

# Build executable
$(TARGET): $(OBJECTS)
	$(FC) $(FFLAGS) -o $(TARGET) $(OBJECTS)

# Compile source files
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

# Debug build
debug: FFLAGS = $(DEBUGFLAGS)
debug: clean $(TARGET)

# Install target (optional)
install: $(TARGET)
	install -m 755 $(TARGET) /usr/local/bin/

# Clean build artifacts
clean:
	rm -f $(OBJECTS) $(TARGET) *.mod
	rm -f /tmp/fuss_*.txt

# Run the program
run: $(TARGET)
	./$(TARGET)

# Run with --all flag
run-all: $(TARGET)
	./$(TARGET) --all

# Phony targets
.PHONY: all clean install run run-all debug
