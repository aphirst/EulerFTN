# This file is part of EulerFTN.
# Copyright (C) 2013-2014 Adam Hirst <adam@aphirst.karoo.co.uk>
#
# EulerFTN is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# EulerFTN is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with EulerFTN. If not, see <http://www.gnu.org/licenses/>.

SRCS_f90d1 = \
functions.f90 \
main.f90 \
euler.f90 \
constants.f90

OBJS_f90d1 = \
functions.o \
main.o \
euler.o \
constants.o

SRC_DIR_f90d1 =
OBJS_DIR = /tmp/EulerFTN/obj/
EXE_DIR = /tmp/EulerFTN/

EXE = EulerFTN
FC = gfortran
IDIR =
CFLAGS = -std=f2008 -Wextra -Wall -O2 -s -march=native -J$(OBJS_DIR) $(IDIR)
LFLAGS =
LIBS =

VPATH = $(SRC_DIR_f90d1):$(OBJS_DIR)
OBJS = $(addprefix $(OBJS_DIR), $(OBJS_f90d1))

all : $(EXE)

$(EXE) : $(OBJS_f90d1)
	@mkdir -p $(EXE_DIR)
	$(FC) -o $(EXE_DIR)$(EXE) $(OBJS) $(LFLAGS) $(LIBS)

$(OBJS_f90d1):
	@mkdir -p $(OBJS_DIR)
	$(FC) $(CFLAGS) -c $(SRC_DIR_f90d1)$(@:.o=.f90) -o $(OBJS_DIR)$@

clean :
	rm -f $(OBJS_DIR)*.*
	rm -f $(EXE_DIR)$(EXE)

# Dependencies of files
functions.o: \
    functions.f90 \
    constants.o
main.o: \
    main.f90 \
    euler.o
euler.o: \
    euler.f90 \
    functions.o
constants.o: \
    constants.f90

