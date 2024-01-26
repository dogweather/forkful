---
title:                "複素数の扱い方"
date:                  2024-01-26T04:40:36.126549-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
複素数は実部と虚部(`a + bi`)を持っています。これらは電気工学や量子コンピューティングなど、さまざまな分野で便利です。プログラマーは、実数だけを使用して解決できない方程式をモデル化するためにそれらを使用します。

## 方法：
Gleamには複素数のネイティブサポートがありません。通常、独自のものを作成するか、ライブラリを探します。基本的な操作を実装する方法の簡単な例をここに示します：

```gleam
type Complex {
  Complex(Float, Float)
}

fn add(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a + x, b + y)
}

fn multiply(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a*x - b*y, a*y + b*x)
}

fn main() {
  let num1 = Complex(1.0, 2.0)
  let num2 = Complex(3.0, 4.0)
  let sum = add(num1, num2)
  let product = multiply(num1, num2)

  sum // Complex(4.0, 6.0)
  product // Complex(-5.0, 10.0)
}
```

## 深く掘り下げる

複素数は最初にジェロラモ・カルダノによって16世紀により正式に文書化されました。それらは実数の自然な拡張です。しかしながら、パフォーマンスとタイプセーフティーを優先する若い言語であるGleamでは、そのような機能は最小限です（またはDIYです）。

Pythonのように、他の言語では複素数は組み込まれています(`3+4j`)、これは生活を楽にします。RustやHaskellでは、箱から出してすぐに高度な機能を提供するライブラリがあります。

Gleamのアプローチでは、すべての側面を処理する必要があります：算術、極座標、指数形式など。効率的で正確な操作を実装するには、浮動小数点の振る舞いが結果にどのように影響を与えるかを考慮して慎重なプログラミングが必要です。

特にエッジケースでは、十分にテストすることを忘れないでください！複素数の無限大やNaN（数値ではない）値を処理する際に、注意しないとつまずく可能性があります。

## 参照
さらに役立つ情報については、ここで深く掘り下げることができます：
- [Gleamの公式ドキュメント](https://gleam.run/documentation/)
- Rustの[num-complex](https://crates.io/crates/num-complex)やPythonの[cmathモジュール](https://docs.python.org/3/library/cmath.html)など、他言語のライブラリからインスピレーションを得る。