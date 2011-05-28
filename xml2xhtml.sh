#!/bin/sh

# Copyright 2010, 2011 Pavel Lepin
# 
# This file is part of Data.TotalRecall.
# 
# Data.TotalRecall is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Data.TotalRecall is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Data.TotalRecall.  If not, see <http://www.gnu.org/licenses/>.

xsltproc --stringparam files "`echo *.xml`" xf.xsl xf.xsl

