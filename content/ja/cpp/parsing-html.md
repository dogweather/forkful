---
title:                "HTMLの解析"
html_title:           "Arduino: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/parsing-html.md"
---

{{< edit_this_page >}}

# C++によるHTMLのパース: 一歩ずつ学ぼう

## 何と何故？
HTMLのパースとは、HTMLの書き方と構造を解析することです。この技能はウェブクローラーやウェブスクレイパーの製作、またはHTML文書の自動的な修正や操作等、プログラマーに様々な可能性を開くために求められます。

## 実践: 
我々のためにGumboというライブラリが用意されています。HTMLを解析して、いかなるタグや属性も簡単にアクセスできるようにします。 例えば、次のようになります:

```C++
#include <gumbo.h>

int main() {
    const char* html = "<html><body><h1>はじめまして、世界！</h1></body></html>";
    GumboOutput* output = gumbo_parse(html);

    GumboNode* root = output->root;
    // rootから情報を探索

    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

## 掘り下げ
**歴史的文脈:** HTMLパーサーはウェブの歴史と深く結びついており、最初のパーサーはテキストベースのブラウザであるLynxで使用されていました。しかし、現代のHTMLはそのサイズ、複雑性、そして互換性の問題により、特殊なツールが必要としています。

**代替手段:** HTMLのパースには他の方法もあります。例えば、PythonのBeautifulSoup、JavaのJsoup、そしてJavaScriptのJSDOMなどが挙げられます。選択は主にプログラムの要件とパーソナルな好みによるものです。

**実装の詳細:** HTMLパーサーの実装は、文法解析とDOMツリーの構築の両方を含め、非常に複雑です。例えば、GumboはHTML5の仕様に完全に準拠していますが、他のパーサーでは互換性や実用性のために仕様から逸脱することもあります。

## 参考文献
- [GumboライブラリのGitHubページ](https://github.com/google/gumbo-parser)
- [HTML5パーサーの比較](https://html5test.com/)
- [BeautifulSoupの公式ドキュメンテーション](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Jsoupの公式ドキュメンテーション](https://jsoup.org/)
- [JSDOMの公式ドキュメンテーション](https://github.com/jsdom/jsdom)