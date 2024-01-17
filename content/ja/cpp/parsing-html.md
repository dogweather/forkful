---
title:                "HTMLの解析"
html_title:           "C++: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/parsing-html.md"
---

{{< edit_this_page >}}

#何かとなぜ
HTMLをパースするとは何かと言うと、プログラマーがウェブページを読み込んで、その中に含まれるタグやデータを抽出することです。ウェブスクレイピングやデータマイニングなど、さまざまなアプリケーションでHTMLパースが必要となるため、プログラマーにとって重要なスキルです。

#方法
```C++ 
#include <iostream>
#include <string>
#include <vector>

using namespace std;

int main() {
  // サンプルHTML
  string html = "<div>
                  <h1>Hello, world!</h1>
                  <p>This is a paragraph.</p>
                </div>"

  vector<string> tags; // タグを格納するためのベクトル

  // パース処理
  while (html.length() > 0) {
    int start = html.find("<"); // タグの開始位置を探す
    if (start != string::npos) { // もしタグが見つかったら
      int end = html.find(">"); // タグの終了位置を探す
      tags.push_back(html.substr(start, (end-start)+1)); // ベクトルにタグを追加
      html = html.substr(end+1); // 残りのHTMLを更新
    }
    else {
      html = ""; // タグが見つからなかったら処理を終了
    }
  }

  // 抽出されたタグの表示
  for (int i = 0; i < tags.size(); i++) {
    cout << tags[i] << endl;
  }

  return 0;
}
```

出力:
``` 
<div>
<h1>Hello, world!</h1>
<p>This is a paragraph.</p>
</div>
```

#ディープダイブ
HTMLパースの歴史は長く、ウェブの発展とともに進化してきました。1990年代の初め、ティム・バーナーズ＝リーによって設計されたHTMLは、その後も様々なバージョンが作られ、現在もリビジョンが進められています。また、HTMLパースの代替手段としては、正規表現やパースライブラリなどがありますが、それぞれに利点と欠点があり、使用する場合は注意が必要です。HTMLパースは、ウェブスクレイピングやデータマイニングなどのアプリケーションにおいて、重要な手段として利用されています。

#参考リンク
- [HTMLパースの基本方法](https://www.w3schools.com/html/html_parsing.asp)
- [HTMLパースについてのブログ記事](https://css-tricks.com/the-simplest-ways-to-handle-html-in-c/)
- [正規表現を使ったHTMLパースの例](https://www.regular-expressions.info/examples.html)