/**
 * $File: util.js $
 * $Date: 2023-11-15 16:27:46 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2023 by Shen, Jen-Chieh $
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
