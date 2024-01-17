---
title:                "デバッグ出力のプリント"
html_title:           "Fish Shell: デバッグ出力のプリント"
simple_title:         "デバッグ出力のプリント"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

# Fish シェルでデバッグ出力をプリントする方法

## なに？なぜ？
デバッグ出力をプリントするとは、プログラマーがコードがどのように動いているかを理解し、問題を解決するために行うアクションです。プログラマーはコードの実行中に変数の値やメソッドの実行結果などを表示することで、コードの挙動をより詳細に把握することができます。

## 使い方：
**例1：** 変数の値をプリントする
```Fish Shell
echo $var
```
```
Output: value
```
**例2：** メソッドの戻り値をプリントする
```Fish Shell
function add
  echo $argv[1] + $argv[2]
end
add 2 3
```
```
Output: 5
```

## 詳細について
デバッグ出力をプリントすることは、コードの実行中に起きた問題を特定するのに役立ちます。以前はデバッグ出力を表示するためには、コード内に記述する必要がありましたが、Fish シェルでは `echo` コマンドを使用することで簡単にデバッグ出力をプリントすることができます。

他のシェルと比べると、Fish シェルの `echo` コマンドはより簡潔かつ使いやすいものです。また、Fish シェルでは環境変数である `$fish_trace` を設定することで、コマンドや関数の呼び出し履歴を出力することもできます。

## 関連情報
- [`echo` command documentation](https://fishshell.com/docs/current/cmds/echo.html)
- [`$fish_trace` documentation](https://fishshell.com/docs/current/variables.html#fishtrace)