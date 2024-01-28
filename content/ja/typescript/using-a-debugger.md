---
title:                "デバッガーの使い方"
date:                  2024-01-26T04:11:26.144989-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？
デバッガは、コードが実行されている間にコードの内部動作を調査し、変更するためのツールです。プログラマーは、コードをステップ実行してバグを取り除き、変数を検査し、プログラムの流れを理解するためにこれを使用します。

## 方法：

TypeScriptでデバッガを使い始めるには、サポートされているIDE（Visual Studio Codeなど）と`launch.json`設定が必要です。こちらはNode.jsアプリケーションのための簡単な例です：

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Hello, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

これをデバッグするには、`.vscode`フォルダの下に`launch.json`ファイルを作成します：

```JSON
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "launch",
            "name": "Launch Program",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

次に、IDEの行番号の左側をクリックして`greet`関数にブレークポイントを設定します。F5キーを押してデバッグを開始し、アプリがブレークポイントで一時停止するのを見ます。これで、変数をホバーしたり、式を監視したり、コードをステップ実行するのが簡単になります。

## 深掘り

かつて統合開発環境（IDE）が洗練される前は、デバッグはしばしばプリントステートメント（いわゆる`console.log`デバッグ）で行われていました。それなりに機能しましたが、目隠しをして干し草の山から針を見つけるようなものでした。

現代のデバッガは、トラブルシューティングのためのスイスアーミーナイフのようなものです。TypeScriptとNode.jsの進化に伴い、組み込みのNode.jsインスペクタからクライアントサイドデバッグのためのブラウザ開発ツールまで、さまざまなデバッガが利用可能です。

Node.jsインスペクタは、実行中のアプリケーションにアタッチして動作し、Chrome DevTools Protocol経由で通信します。これによって、Chromeブラウザが強力なデバッグコンソールに変わります。この統合により、従来のコマンドラインデバッグプラクティスと比較してビジュアル的にインタラクティブかつ詳細なデバッグセッションが可能になります。

## 参照

さらに読むために、そしてプロのヒントのために、こちらをチェックしてください：

- [Visual Studio CodeでのTypeScriptデバッグ](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Node.jsデバッグガイド](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Chrome DevToolsドキュメント](https://developers.google.com/web/tools/chrome-devtools)
