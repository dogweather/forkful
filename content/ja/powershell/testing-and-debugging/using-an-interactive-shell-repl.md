---
date: 2024-01-26 04:17:08.156608-07:00
description: "\u5BFE\u8A71\u578B\u30B7\u30A7\u30EB\u3001\u307E\u305F\u306F Read-Eval-Print\
  \ Loop (REPL) \u3092\u4F7F\u7528\u3059\u308B\u3068\u3001PowerShell\u2026"
lastmod: '2024-03-13T22:44:42.439331-06:00'
model: gpt-4-0125-preview
summary: "\u5BFE\u8A71\u578B\u30B7\u30A7\u30EB\u3001\u307E\u305F\u306F Read-Eval-Print\
  \ Loop (REPL) \u3092\u4F7F\u7528\u3059\u308B\u3068\u3001PowerShell\u2026"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となく何故？
対話型シェル、または Read-Eval-Print Loop (REPL) を使用すると、PowerShell コマンドを入力し、即座にフィードバックを得ることができます。プログラマーは、コードスニペットを素早くテストしたり、デバッグしたり、フルスクリプトを書かずに新しいコマンドを学んだりするためにこれを利用します。

## 方法：
PowerShellを起動すると、REPLに入ります。`Get-Date` Cmdletを試してみましょう：

```PowerShell
PS > Get-Date
```

現在の日付と時刻が出力されるはずです：

```PowerShell
2023年3月31日 水曜日 12時34分56秒
```

次に、コマンドを連鎖させましょう。メモリ使用量でプロセスをソートしてみましょう：

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

これにより、ワーキングセットサイズ（メモリ使用量）でトップ5のプロセスが出力されます。

## 深堀り
PowerShell の REPL は、Unix シェルや Python のような他の動的言語シェルにその起源を持ちます。それは単一ユーザーの対話型コマンド実行環境です。コンパイル言語では、アプリケーション全体を書いてからコンパイルするのに対し、REPL 環境では、一度に 1 行のコードを書いて実行することができます。PowerShellは、より大規模なタスクのためのスクリプト実行もサポートしています。

Windowsでは、コマンドプロンプトや IPython のような言語固有の REPL などの代替品があります。Unix/Linux の世界では、bash や zsh などのシェルが同様の機能を提供します。

PowerShell の実装は、ホストアプリケーションを使用してシェルを実行します。Windowsの PowerShell.exe が最も一般的ですが、Integrated Scripting Environment (ISE) や Visual Studio Code の統合ターミナルもホストとして機能します。

## 参照
- [PowerShell について](https://docs.microsoft.com/ja-jp/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
