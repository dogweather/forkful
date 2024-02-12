---
title:                "デバッガーの使い方"
aliases:
- /ja/c-sharp/using-a-debugger/
date:                  2024-01-26T03:48:54.730715-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？
デバッガーを使用するということは、コードをテストし診断するための特殊なツールを利用することを意味します。プログラマーがバグを潰し、コードフローを理解し、コードが期待通りに振る舞うことを確認するためにこれを行います—これは、あなたのコードの「脳」のための顕微鏡のようなものです。

## 方法：
想像してみてください、あなたには正しく動作していない小さなプログラムがあります：

```C#
static void Main()
{
    int result = Sum(1, 2);
    Console.WriteLine(result);
}

static int Sum(int a, int b)
{
    return a + a; // おっと、ここはa + bであるべきです
}
```

Visual Studioのデバッガーを使用して、`return a + a;`の隣にある左マージンをクリックしてブレークポイントを設定します。プログラムを実行するとき（F5キーで）、実行はそこで一時停止します。変数の上にマウスカーソルを置いて値を調べるか、または即時ウィンドウを使用して式を評価してみてください。`a` が1で、`b` が2であることがわかりますが、`a + a`は期待した合計ではありません。それを`a + b`に変えて、実行を続ける（F5キー）、そして、ほら、コンソールが3を出力します。

## ディープダイブ
デバッグの歴史は1940年代まで遡り、初期のコンピューターで本物のバグ（蛾）が発見されたときに始まります。今日のデバッガー、Visual Studioのものと同様、ブレークポイント、ステップバイステップ実行、ウォッチウィンドウなど、強力な機能のスイートを提供します。

Visual Studioのデバッガーに代わるオプションには、Cスタイルの言語用のGDBやPython用のpdbなどのオープンソースオプション、またC#および他の言語のデバッグツールを提供するクロスプラットフォームIDEであるJetBrains RiderやVS Codeが含まれます。

デバッガーの実装に深く飛び込むとき、あなたが見るのは、アプリケーションのプロセスにアタッチするプログラムです。それは機械語を解釈し、メモリ状態を管理し、実行フローを制御します。これは、効果的なデバッグのためには欠かせない重要なものであり、デバッグモードがリリースモードよりも遅く動作する理由でもあります。これらのフックが存在しないからです。

## 参照
- [Visual Studio デバッガー ドキュメント](https://docs.microsoft.com/en-us/visualstudio/debugger/)
- [デバッグ戦略](https://www.codeproject.com/Articles/79508/Effective-Exception-Handling-in-Visual-C)
