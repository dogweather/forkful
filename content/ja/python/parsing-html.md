---
title:                "Python: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/parsing-html.md"
---

{{< edit_this_page >}}

## なぜ
HTML解析を行う理由は何ですか？HTMLはウェブサイトの構造やデータを表すために使用される言語です。Pythonのプログラミングを使用して、HTMLを解析することで、ウェブサイトから必要な情報を取得することができます。

## 方法
まずは、HTMLを解析するために必要なツールをインストールしましょう。PythonではBeautiful Soupというツールが一般的に使用されています。以下のコードを使用することで、HTMLを解析する準備が整います。

```python
from bs4 import BeautifulSoup
```

次に、解析したいHTMLのURLを指定し、Beautiful Soupを使用してHTMLを取得します。

```python
url = "https://example.com"
html = requests.get(url).html
```

そして、取得したHTMLをBeautiful Soupのオブジェクトとして読み込みます。

```python
soup = BeautifulSoup(html, 'html.parser')
```

これで解析準備は完了です。例えば、ウェブサイトから特定のタグのテキストを取得する場合は以下のようにコードを記述します。

```python
tag = soup.find('p') # pタグを取得
print(tag.text) # pタグ内のテキストを表示
```

## 詳細情報
HTML解析を行う際には、正しいセレクターを使用することが重要です。セレクターとは、HTMLの要素を選択するためのパターンです。Beautiful SoupではCSSセレクターを使用することができます。詳しくは以下のリンクを参考にしてください。

- [Beautiful Soupの公式ドキュメント](https://www.crummy.com/software/BeautifulSoup/bs4/doc/ja/)

また、Beautiful Soupを使用しても解析できない場合は、HTMLをより詳細に分析する必要があります。その際には、Pythonの正規表現を使用することで、柔軟にHTMLを解析することができます。正規表現については以下のリンクを参考にしてください。

- [Python正規表現チュートリアル](https://docs.python.org/ja/3/howto/regex.html)

## 同様に参照してください
- [Beautiful Soup公式ドキュメント（日本語）](https://www.crummy.com/software/BeautifulSoup/bs4/doc/ja/)
- [Python正規表現チュートリアル（日本語）](https://docs.python.org/ja/3/howto/regex.html)