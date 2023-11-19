/**
 * Copyright (C) 2023 the Ellsp authors.
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

const env = require('./env');

const child_process = require("child_process");
const stream = require('stream');

/**
 * Start Emacs process.
 */
function startEmacs() {
  if (!UTIL.which(ELLSP_EMACS)) {
    console.log("Emacs is not installed (cannot find `emacs' executable)");
    return null;
  }

  let proc = child_process
      .spawn(ELLSP_EMACS,
             ['--batch', '--no-init-file', '--no-site-file', '--no-splash',
              '--load=ellsp',
              '--funcall=ellsp-stdin-loop'],
             { stdio: ['pipe', 'inherit', 'inherit'], shell: true }
            );

  proc.on('close', function (code) {
    if (code == 0) return;
    process.exit(code);
  });

  return proc;
}

/**
 * Program entry.
 */
function main() {
  let proc = startEmacs();
  if (proc === null)
    return;

  process.stdin.on('data', function(data) {
    //console.error(data.toString());
    let input = data.toString();
    if (input.includes('}Content-Length: '))
      
    proc.stdin.write(input + '\r\n');
  });
}

main();  // Start the program!
