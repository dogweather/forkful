---
title:                "複素数の扱い方"
date:                  2024-01-26T04:36:52.254965-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"

category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
複素数は実部と虚部で構成されます。プログラマーは、通常の実数では不十分な場合、信号処理、量子力学、また他の計算が必要な場面でこれを使用します。

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
