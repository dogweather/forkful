---
date: 2024-01-26 03:46:13.135047-07:00
description: "\u6570\u5B57\u306E\u4E38\u3081\u64CD\u4F5C\u306F\u3001\u5024\u3092\u6700\
  \u3082\u8FD1\u3044\u6574\u6570\u3042\u308B\u3044\u306F\u6307\u5B9A\u3055\u308C\u305F\
  \u5C0F\u6570\u70B9\u4EE5\u4E0B\u306E\u6841\u307E\u3067\u8ABF\u6574\u3059\u308B\u3053\
  \u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\
  \u3001\u30C7\u30FC\u30BF\u3092\u7C21\u7D20\u5316\u3057\u3001\u8AAD\u307F\u3084\u3059\
  \u3055\u3092\u5411\u4E0A\u3055\u305B\u308B\u3001\u307E\u305F\u306F\u8A08\u7B97\u4E2D\
  \u306B\u7279\u5B9A\u306E\u6570\u5B66\u7684\u8981\u4EF6\u3092\u6E80\u305F\u3059\u305F\
  \u3081\u306B\u3001\u6570\u5B57\u3092\u4E38\u3081\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.427408-06:00'
model: gpt-4-0125-preview
summary: "\u6570\u5B57\u306E\u4E38\u3081\u64CD\u4F5C\u306F\u3001\u5024\u3092\u6700\
  \u3082\u8FD1\u3044\u6574\u6570\u3042\u308B\u3044\u306F\u6307\u5B9A\u3055\u308C\u305F\
  \u5C0F\u6570\u70B9\u4EE5\u4E0B\u306E\u6841\u307E\u3067\u8ABF\u6574\u3059\u308B\u3053\
  \u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\
  \u3001\u30C7\u30FC\u30BF\u3092\u7C21\u7D20\u5316\u3057\u3001\u8AAD\u307F\u3084\u3059\
  \u3055\u3092\u5411\u4E0A\u3055\u305B\u308B\u3001\u307E\u305F\u306F\u8A08\u7B97\u4E2D\
  \u306B\u7279\u5B9A\u306E\u6570\u5B66\u7684\u8981\u4EF6\u3092\u6E80\u305F\u3059\u305F\
  \u3081\u306B\u3001\u6570\u5B57\u3092\u4E38\u3081\u307E\u3059\u3002"
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

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
