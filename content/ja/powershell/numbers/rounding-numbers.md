---
date: 2024-01-26 03:46:13.135047-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A PowerShell\u306B\u306F\u3001\u4E38\
  \u3081\u64CD\u4F5C\u306E\u305F\u3081\u306E\u3044\u304F\u3064\u304B\u306E\u4FBF\u5229\
  \u306Acmdlets\u3068\u30E1\u30BD\u30C3\u30C9\u304C\u3042\u308A\u307E\u3059\uFF1A\
  \ - Math\u30AF\u30E9\u30B9\u304B\u3089\u306E`Round()`\u30E1\u30BD\u30C3\u30C9."
lastmod: '2024-04-05T21:53:43.251221-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

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
