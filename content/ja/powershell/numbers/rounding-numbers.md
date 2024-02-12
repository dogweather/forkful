---
title:                "数値の丸め処理"
date:                  2024-01-26T03:46:13.135047-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/rounding-numbers.md"
---

{{< edit_this_page >}}

## 何となく何故？
数字の丸め操作は、値を最も近い整数あるいは指定された小数点以下の桁まで調整することについてです。プログラマは、データを簡素化し、読みやすさを向上させる、または計算中に特定の数学的要件を満たすために、数字を丸めます。

## どうやって：
PowerShellには、丸め操作のためのいくつかの便利なcmdletsとメソッドがあります：

- Mathクラスからの`Round()`メソッド
```PowerShell
[Math]::Round(15.68) # 16に丸める
```
- 小数点以下を指定：
```PowerShell
[Math]::Round(15.684, 2) # 15.68に丸める
```
- 常に丸め上げる`Ceiling()`と丸め下げる`Floor()`：
```PowerShell
[Math]::Ceiling(15.2) # 16に丸め上げる
[Math]::Floor(15.9) # 15に丸め下げる
```

## 詳細な解説
数字の丸め操作は新しいものではなく、古代から貿易、科学、時間計測に役立ってきました。PowerShellについて言えば、`[Math]::Round()`はデフォルトで"銀行家の丸め"に従います。ここでは、0.5が最も近い偶数に丸められるため、統計操作における偏りが減少します。

しかし、`[Math]`メソッドに限定されているわけではありません。もっと制御が欲しいですか？`[System.Math]::Round(Number, Digits, MidpointRounding)`を確認してみてください。ここでは中点の扱い方を設定できます：ゼロから離れてあるいは偶数に向かって（いわゆる銀行家の丸め）。

別の角度から：`System.Globalization.CultureInfo`オブジェクトです。それは、国際的な数字を扱う際の地域固有のフォーマットや丸めの好みに役立ちます。

## 参照
- Microsoftの公式ドキュメント上のMathメソッド：[リンク](https://learn.microsoft.com/en-us/dotnet/api/system.math?view=net-7.0)
- .NETにおける小数点の丸めの具体的な事項：[リンク](https://learn.microsoft.com/en-us/dotnet/api/system.midpointrounding?view=net-7.0)
- StackOverflow上の丸めに関する議論：[リンク](https://stackoverflow.com/questions/tagged/rounding+powershell)
