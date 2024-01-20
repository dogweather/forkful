---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# PowerShellによる乱数の生成について

## 何について？なぜ必要なのか ?

乱数生成とはランダムな数値を作り出すプロセスのことを指します。プログラマが乱数を使用する理由は多岐にわたり、例えばゲームのプレイシンアリオを多様化したり、テストデータを生成したり、暗号化の質を向上させるためなどといった具体的な利用シーンがあります。

## 実行方法:

PowerShellを使って乱数を生成する一番簡単な方法は Get-Random コマンドレットを利用することです。

```PowerShell
# 1から10までの間で乱数を生成
Get-Random -Min 1 -Max 10
```

出力例:

``` PowerShell
7
```

なお、乱数は独立するため、上記のコードを再実行すると異なる結果が返されます。

また、指定した範囲の乱数をリストとして生成することも可能です。

```PowerShell
# 1から50までの間で5つの乱数を生成
1..5 | ForEach-Object { Get-Random -Min 1 -Max 50 }
```

出力例:

```PowerShell
45
7
23
38
12
```

## より深く知るために:

乱数生成について語る上で、過去の文脈に触れるのは重要です。当初、乱数生成は物理的な方法（コインの投げ、ダイスの振りなど）によって行われていました。しかし今日では計算機による方法が主流となり、より速く、より大きな量の乱数を生成可能になりました。  

PowerShellではGet-Randomコマンドレットの他に、System.Randomクラスも乱数を生成するための方法として用意されています。このクラスを使用すると、乱数のシード値を指定して、同一の乱数シークエンスを生成することができます。  

```PowerShell
# System.Randomクラスの新しいインスタンスを作成して乱数を生成する
$random = New-Object System.Random
$random.Next(1, 100)
```

また、PowerShellの乱数生成は擬似乱数生成器を使用しています。これは完全にランダムな数値を生成するのではなく、特定の初期値（乱数シード）から始まる数列を生成することで乱数を模倣します。

## 参考情報：

- [Get-Random](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.1)
- [System.Random](https://docs.microsoft.com/ja-jp/dotnet/api/system.random?view=net-5.0)
- [乱数生成の歴史](http://www.random.org/history)