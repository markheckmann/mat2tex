### This file is part of the mat2tex package for R. It is made
### available under the terms of the GNU General Public License,
### version 3, or at your option, any later version, incorporated
### herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE. See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA


# add default options for package
.onAttach <- function(lib, pkg){
  mat2tex_options_init()
}


# clean up options when package is detached
.onUnload <- function(lib){
  mat2tex_remove_options()
} 
