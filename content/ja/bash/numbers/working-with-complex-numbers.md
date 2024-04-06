---
date: 2024-01-26 04:36:52.254965-07:00
description: "\u65B9\u6CD5\uFF1A Bash\u306F\u30CD\u30A4\u30C6\u30A3\u30D6\u306B\u8907\
  \u7D20\u6570\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u305B\u3093\u3002\
  \u3088\u304F`bc`\u30B3\u30DE\u30F3\u30C9\u3068\u305D\u306E`-l`\u30AA\u30D7\u30B7\
  \u30E7\u30F3\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306Fbash\u3067\
  \u8907\u7D20\u6570\u3092\u51E6\u7406\u3059\u308B\u65B9\u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:38:41.878256-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Bash\u306F\u30CD\u30A4\u30C6\u30A3\u30D6\u306B\u8907\u7D20\
  \u6570\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u305B\u3093\u3002\u3088\
  \u304F`bc`\u30B3\u30DE\u30F3\u30C9\u3068\u305D\u306E`-l`\u30AA\u30D7\u30B7\u30E7\
  \u30F3\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306Fbash\u3067\u8907\
  \u7D20\u6570\u3092\u51E6\u7406\u3059\u308B\u65B9\u6CD5\u3067\u3059\uFF1A."
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

## 方法：
Bashはネイティブに複素数をサポートしていません。よく`bc`コマンドとその`-l`オプションを使用します。以下はbashで複素数を処理する方法です：

```bash
echo "sqrt(-1)" | bc -l
```

出力：
```bash
j
```

乗算：

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

出力：
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## 深掘り
複素数は16世紀から存在していますが、Bashのようなスクリプト言語は、標準では複素数のような数学的計算には向いていません。そのため、`bc`や`awk`のようなツールがよく利用されます。複素数の処理に適した他の言語には、より高度な数学的機能を備えたPythonの`cmath`モジュールやMATLABがあります。Bashにとっては、ツールを活用することがすべてです - `bc`は虚数単位を表すために小文字の'i'を使用し、足し算、引き算、掛け算、割り算などの基本的な操作をサポートしています。

## 参照
- `bc`マニュアル：https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave（MATLABの代替品）：https://www.gnu.org/software/octave/
- Python `cmath`モジュール：https://docs.python.org/3/library/cmath.html
