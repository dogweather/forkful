---
date: 2024-01-26 04:11:26.144989-07:00
description: "\u30C7\u30D0\u30C3\u30AC\u306F\u3001\u30B3\u30FC\u30C9\u304C\u5B9F\u884C\
  \u3055\u308C\u3066\u3044\u308B\u9593\u306B\u30B3\u30FC\u30C9\u306E\u5185\u90E8\u52D5\
  \u4F5C\u3092\u8ABF\u67FB\u3057\u3001\u5909\u66F4\u3059\u308B\u305F\u3081\u306E\u30C4\
  \u30FC\u30EB\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\
  \u30FC\u30C9\u3092\u30B9\u30C6\u30C3\u30D7\u5B9F\u884C\u3057\u3066\u30D0\u30B0\u3092\
  \u53D6\u308A\u9664\u304D\u3001\u5909\u6570\u3092\u691C\u67FB\u3057\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u306E\u6D41\u308C\u3092\u7406\u89E3\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.764231-06:00'
model: gpt-4-0125-preview
summary: "\u30C7\u30D0\u30C3\u30AC\u306F\u3001\u30B3\u30FC\u30C9\u304C\u5B9F\u884C\
  \u3055\u308C\u3066\u3044\u308B\u9593\u306B\u30B3\u30FC\u30C9\u306E\u5185\u90E8\u52D5\
  \u4F5C\u3092\u8ABF\u67FB\u3057\u3001\u5909\u66F4\u3059\u308B\u305F\u3081\u306E\u30C4\
  \u30FC\u30EB\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\
  \u30FC\u30C9\u3092\u30B9\u30C6\u30C3\u30D7\u5B9F\u884C\u3057\u3066\u30D0\u30B0\u3092\
  \u53D6\u308A\u9664\u304D\u3001\u5909\u6570\u3092\u691C\u67FB\u3057\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u306E\u6D41\u308C\u3092\u7406\u89E3\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
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
