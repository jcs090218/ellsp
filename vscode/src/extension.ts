// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import { workspace } from 'vscode';
import * as vscode from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	Trace,
} from 'vscode-languageclient/node';

let client: LanguageClient;

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {
	const serverExecutable = { command: 'eask', args: ['exec', 'ellsp'] };

	const serverOptions: ServerOptions = {
		run: serverExecutable,
		debug: serverExecutable
	};

	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'emacs-lisp' }],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/*.el')
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'ellsp', 'ellsp',
		serverOptions,
		clientOptions
	);

	// Register the commands
	context.subscriptions.push(vscode.commands.registerCommand('ellsp.start', start));
	context.subscriptions.push(vscode.commands.registerCommand('ellsp.stop', stop));

	// Start the client. This will also launch the server
	{
		client.setTrace(Trace.Verbose);
		client.start();
	}
}

// This method is called when your extension is deactivated
export function deactivate() {
	if (!client) {
		return undefined;
	}
	return client.stop();
}

async function start() {
	client.start();
	console.log('[INFO] `ellsp` is running!');
}


async function stop() {
	deactivate();
	console.log('[INFO] `ellsp` has been shutdown!');
}
