---
title:                "複素数を操作する"
date:                  2024-02-03T18:14:20.877567-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数を操作する"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/working-with-complex-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

プログラミングにおける複素数の扱いとは、実部と虚部の両方を持つ数、典型的には `a + bi` と表現される数の操作を意味します。プログラマーは、負の数の平方根、波形分析など、さまざまな問題を解決するために、工学、物理学、データ分析などのさまざまな分野で複素数に取り組みます。

## 方法：

Goでは、組み込みの`complex`、`real`、`imag`関数と、`complex64`及び`complex128`型（それぞれ64ビットと128ビットの複素数を表す）を使用して複素数が扱われます。ここにクイックスタートガイドを示します：

```go
package main

import (
	"fmt"
)

func main() {
	// 複素数の作成
	a := complex(2, 3) // 2+3i
	b := complex(1, -1) // 1-1i

	// 算術演算
	c := a + b
	fmt.Println("加算:", c) // 出力: 加算: (3+2i)

	d := a * b
	fmt.Println("乗算:", d) // 出力: 乗算: (5+1i)

	// 実部と虚部のアクセス
	realPart := real(a)
	imagPart := imag(a)
	fmt.Printf("実部: %.1f, 虚部: %.1f\n", realPart, imagPart) // 出力: 実部: 2.0, 虚部: 3.0

	// 共役と大きさは計算できる
	conjugate := complex(real(a), -imag(a)) // 手動で
	fmt.Println("aの共役:", conjugate) // 出力: aの共役: (2-3i)
}

```

この例では基本をカバーしていますが、`math/cmplx`パッケージを活用することで、大きさや位相を見つけるなど、もっと多くのことを複素数でできます。

## 深掘り

複素数の概念は16世紀にさかのぼりますが、19世紀に広く認識され、厳密な形式化がなされました。コンピュータープログラミングにおいて、複素数は初期から科学的および工学的計算の複雑な算術のための定番です。Goによる複素数の扱いは、組み込みサポートを備え、`math/cmplx`パッケージを通じて包括的な標準ライブラリーサポートを提供することで、プログラミング言語の中で際立っています。この設計決定は、Goのシンプルさと性能への重点を反映しています。

それにもかかわらず、Goでの複素数の扱いが強力であるにせよ、シンボリック数学や高精度算術が必要な全てのアプリケーションに最適とは限らない、ということに注意する価値があります。科学計算に特化した言語や環境、例えばNumPyやSciPyのようなライブラリーを持つPythonや、MATLABのようなソフトウェアは、特定のアプリケーションに対してより多くの柔軟性と機能範囲を提供するかもしれません。

それにも関わらず、システムプログラミングや、より大きな、パフォーマンスに敏感なアプリケーションへの複素数計算の統合が重要なコンテキストでは、Goの複素数に対するネイティブサポートは、ユニークで効率的なオプションを提供します。