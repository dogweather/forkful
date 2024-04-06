---
date: 2024-01-20 17:35:54.654331-07:00
description: "How to: (\u3084\u308A\u65B9) \u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\
  \u5909\u63DB\u3059\u308B\u5FC5\u8981\u6027\u306F\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\
  \u521D\u671F\u304B\u3089\u3042\u308A\u307E\u3057\u305F\u3002UNIX\u7CFBOS\u3067\u306F\
  \u3001`date`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.220460-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\
  \u3059\u308B\u5FC5\u8981\u6027\u306F\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u521D\u671F\
  \u304B\u3089\u3042\u308A\u307E\u3057\u305F\u3002UNIX\u7CFBOS\u3067\u306F\u3001`date`\
  \ \u30B3\u30DE\u30F3\u30C9\u304C\u3053\u308C\u3092\u884C\u3046\u6A19\u6E96\u7684\
  \u306A\u30C4\u30FC\u30EB\u3067\u3059\u3002\u5225\u306E\u30AA\u30D7\u30B7\u30E7\u30F3\
  \u306B\u306FPython\u3084Perl\u306A\u3069\u306E\u30B9\u30AF\u30EA\u30D7\u30C8\u8A00\
  \u8A9E\u304C\u3042\u308A\u3001\u591A\u69D8\u306A\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\
  \u30AA\u30D7\u30B7\u30E7\u30F3\u304C\u7528\u610F\u3055\u308C\u3066\u3044\u307E\u3059\
  \u3002Bash\u3067\u306E\u5B9F\u88C5\u3082\u67D4\u8EDF\u6027\u304C\u9AD8\u3044\u3067\
  \u3059\u304C\u3001\u6CE8\u610F\u70B9\u3068\u3057\u3066\u306F\u3001\u30B7\u30A7\u30EB\
  \u3084\u5730\u57DF\u306B\u3088\u3063\u3066 `date` \u30B3\u30DE\u30F3\u30C9\u306E\
  \u6319\u52D5\u304C\u7570\u306A\u308B\u3053\u3068\u304C\u3042\u308A\u307E\u3059\u3002\
  \u4F8B\u3048\u3070\u3001macOS\u306E `date` \u30B3\u30DE\u30F3\u30C9\u306F\u3001\
  Linux\u3067\u4F7F\u3046\u3082\u306E\u3068\u30AA\u30D7\u30B7\u30E7\u30F3\u304C\u7570\
  \u306A\u308B\u3053\u3068\u304C\u3042\u308A\u307E\u3059\u3002\u30B7\u30A7\u30EB\u30B9\
  \u30AF\u30EA\u30D7\u30C8\u3092\u66F8\u304F\u3068\u304D\u306F\u3001\u4E92\u63DB\u6027\
  \u3092\u610F\u8B58\u3057\u3066\u66F8\u304F\u3053\u3068\u304C\u91CD\u8981\u3067\u3059\
  \u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## How to: (やり方)
```Bash
# 現在の日付を YYYY-MM-DD 形式で取得
date_str=$(date '+%Y-%m-%d')
echo $date_str

# 出力例: 2023-03-15
```

```Bash
# 特定のフォーマットで日付を設定 (例: 日本の元号を使う)
jp_date_str=$(date '+令和%y年%m月%d日')
echo $jp_date_str

# 出力例: 令和5年03月15日
```

## Deep Dive (詳細情報)
日付を文字列に変換する必要性はコンピュータ初期からありました。UNIX系OSでは、`date` コマンドがこれを行う標準的なツールです。別のオプションにはPythonやPerlなどのスクリプト言語があり、多様なフォーマットオプションが用意されています。Bashでの実装も柔軟性が高いですが、注意点としては、シェルや地域によって `date` コマンドの挙動が異なることがあります。例えば、macOSの `date` コマンドは、Linuxで使うものとオプションが異なることがあります。シェルスクリプトを書くときは、互換性を意識して書くことが重要です。

## See Also (関連情報)
- GNU Coreutils `date` マニュアル: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- 日本時刻に関する更なる情報: https://www.nict.go.jp/JST/JST5.html
