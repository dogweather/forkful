---
date: 2024-01-26 00:56:03.209646-07:00
description: "\u65B9\u6CD5\uFF1A PowerShell\u306FMonad\u3068\u3057\u3066\u521D\u3081\
  \u3066\u767B\u5834\u3057\u3066\u4EE5\u6765\u3001\u9577\u3044\u9053\u306E\u308A\u3092\
  \u6B69\u3093\u3067\u304D\u307E\u3057\u305F\u3002\u30A8\u30E9\u30FC\u51E6\u7406\u306F\
  \u6642\u9593\u3068\u5171\u306B\u3088\u308A\u5805\u7262\u306B\u306A\u308A\u3001C#\u306E\
  \u3088\u3046\u306A\u4ED6\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\
  \u3068\u4F3C\u305F\u6A5F\u80FD\u3092\u63D0\u4F9B\u3059\u308B\u3088\u3046\u306B\u306A\
  \u308A\u307E\u3057\u305F\u3002`try-catch-\u2026"
lastmod: '2024-04-05T22:38:41.953789-06:00'
model: gpt-4-1106-preview
summary: "PowerShell\u306B\u306F\u3001\u7D42\u4E86\u30A8\u30E9\u30FC\u3068\u975E\u7D42\
  \u4E86\u30A8\u30E9\u30FC\u3068\u3044\u30462\u3064\u306E\u4E3B\u8981\u306A\u30A8\u30E9\
  \u30FC\u30BF\u30A4\u30D7\u304C\u3042\u308A\u307E\u3059\u3002\u7D42\u4E86\u30A8\u30E9\
  \u30FC\u306F`try-catch`\u30D6\u30ED\u30C3\u30AF\u3067\u6355\u6349\u3055\u308C\u306A\
  \u3044\u9650\u308A\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u505C\u6B62\u3055\u305B\u307E\
  \u3059\u304C\u3001\u975E\u7D42\u4E86\u30A8\u30E9\u30FC\u306F`-ErrorAction Stop`\u3092\
  \u6307\u5B9A\u3057\u306A\u3044\u9650\u308A\u505C\u6B62\u3055\u305B\u307E\u305B\u3093\
  \u3002\u3053\u306E\u533A\u5225\u306F\u3001\u30A8\u30E9\u30FC\u304C\u30B9\u30AF\u30EA\
  \u30D7\u30C8\u5168\u4F53\u3092\u505C\u6B62\u3059\u308B\u307B\u3069\u306E\u3082\u306E\
  \u304B\u3001\u5358\u306B\u8A18\u9332\u3057\u3066\u7121\u8996\u3067\u304D\u308B\u3082\
  \u306E\u304B\u3092\u6C7A\u3081\u308B\u4E0A\u3067\u4E0D\u53EF\u6B20\u3067\u3059\u3002"
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

## 方法：
```PowerShell
# 例外を処理する基本的なTry-Catch
try {
    # エラーを引き起こす可能性があるコード
    $result = 1 / 0
} catch {
    # エラーが発生した場合の対処方法
    Write-Host "おっと、エラーが発生しました: $_"
}

# カスタムエラーメッセージの出力
try {
    Get-Item "nonexistentfile.txt" -ErrorAction Stop
} catch {
    Write-Host "ファイルが見つかりませんでした。"
}

# 最後のエラーを調べるための$Error変数の使用
```

## 深く掘り下げて
PowerShellはMonadとして初めて登場して以来、長い道のりを歩んできました。エラー処理は時間と共により堅牢になり、C#のような他のプログラミング言語と似た機能を提供するようになりました。`try-catch-finally`構文はそのような他の言語からの影響を受けたものの一つです。それ以前には、スクリプターは条件のチェックや`$Error`自動変数の使用に大きく依存していました。

PowerShellには、終了エラーと非終了エラーという2つの主要なエラータイプがあります。終了エラーは`try-catch`ブロックで捕捉されない限りスクリプトを停止させますが、非終了エラーは`-ErrorAction Stop`を指定しない限り停止させません。この区別は、エラーがスクリプト全体を停止するほどのものか、単に記録して無視できるものかを決める上で不可欠です。

PowerShellのエラー処理では、`finally`ブロックも利用できます。これは何が起ころうとも-エラーが発生したかどうかに関わらず-実行されます。クリーンアップタスクに最適です。

スクリプトを深く掘り下げるときには、さらに細かい制御が可能となり、特定の例外タイプを扱うことができます。

また、古い方法として、例外をスローすることなくエラーをキャプチャするための`-ErrorVariable`パラメータや、最後の操作が成功したかどうかを教えてくれる`$?`変数もあります。彼らは便利なツールですが、しっかりした`try-catch`に比べれば少し整理されていない感じがします。

## 参照
- [about_Try_Catch_Finally](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
