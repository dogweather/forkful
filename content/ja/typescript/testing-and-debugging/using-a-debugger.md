---
date: 2024-01-26 04:11:26.144989-07:00
description: "TypeScript\u3067\u30C7\u30D0\u30C3\u30AC\u3092\u4F7F\u3044\u59CB\u3081\
  \u308B\u306B\u306F\u3001\u30B5\u30DD\u30FC\u30C8\u3055\u308C\u3066\u3044\u308BIDE\uFF08\
  Visual Studio Code\u306A\u3069\uFF09\u3068`launch.json`\u8A2D\u5B9A\u304C\u5FC5\u8981\
  \u3067\u3059\u3002\u3053\u3061\u3089\u306FNode.js\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u306E\u305F\u3081\u306E\u7C21\u5358\u306A\u4F8B\u3067\u3059\uFF1A ```TypeScript\
  \ // app.ts function\u2026"
lastmod: '2024-03-13T22:44:41.764231-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\u3067\u30C7\u30D0\u30C3\u30AC\u3092\u4F7F\u3044\u59CB\u3081\u308B\
  \u306B\u306F\u3001\u30B5\u30DD\u30FC\u30C8\u3055\u308C\u3066\u3044\u308BIDE\uFF08\
  Visual Studio Code\u306A\u3069\uFF09\u3068`launch.json`\u8A2D\u5B9A\u304C\u5FC5\u8981\
  \u3067\u3059\u3002\u3053\u3061\u3089\u306FNode.js\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u306E\u305F\u3081\u306E\u7C21\u5358\u306A\u4F8B\u3067\u3059\uFF1A ```TypeScript\
  \ // app.ts function\u2026"
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
weight: 35
---

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
