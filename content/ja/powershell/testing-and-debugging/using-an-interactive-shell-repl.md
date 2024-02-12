---
title:                "インタラクティブシェル（REPL）の使用"
aliases: - /ja/powershell/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:17:08.156608-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/using-an-interactive-shell-repl.md"
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
