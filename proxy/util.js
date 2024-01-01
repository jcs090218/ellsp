/**
 * Copyright (C) 2023-2024 the Ellsp authors.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

"use strict";

/**
 * Check to see if a program is installed and exists on the path.
 * @param { String } command - Program name.
 * @return Return path or null if not found.
 */
function which(command) {
  return require('which').sync(command, { nothrow: true });
}


/*
 * Module Exports
 */
module.exports.which = which;
