---
title:                "コードを関数に整理する"
aliases:
- /ja/powershell/organizing-code-into-functions/
date:                  2024-01-26T01:11:55.083867-07:00
model:                 gpt-4-1106-preview
simple_title:         "コードを関数に整理する"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## なぜ、どのようにして?
コードを関数に整理するというのは、特定のタスクを実行するコードの塊をラップし、それに名前を付けることです。これは、コードを再利用可能、読みやすく、保守しやすくするために行われます。同じコードを書き直す代わりに、関数を呼び出します。トラブルシューティングやアップグレードをしたいですか？スクリプトの山をかき分けることなく、関数を微調整します。

## 方法:
2つの数の合計を計算する関数を書いてみましょう。単純ですが、ポイントを説明するには十分です。

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# 5と10を使って関数を呼び出す
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "合計は $sum です"
```

サンプル出力:

```
合計は 15 です
```

## 詳しくは:
PowerShellの関数は、他の言語と同様に、古いニュースです。私たちはFortranの日々からコードを区分けしています。それは「車輪を再発明しない」ことについてです。代替手段？確かに、スクリプトやコマンドレットがあります。しかし、スクリプト内の関数のような整頓された感じや文脈に応じた感覚はありません。

実装方法？関数は、私たちの例のような基本的なものから、スコープ、パイプライン入力などを持つ複雑なものまで様々です。`Advanced Functions`を取り上げてみましょう。彼らは、`[Parameter(Mandatory=$true)]`のような属性を持つパラメータを持つコマンドレットを模倣します。それはPowerShellの柔軟性の一端です。

## 参照
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
