---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 文字パターンの削除：なにとなぜ？

文字パターンの削除とは正規表現や特定の文字列を使い、テキストデータから該当する文字を削除することを指します。文字データのクリーニングや情報の構造を整えるため、プログラマはこのテクニックを使用します。

# 使い方：

Pythonの内蔵関数`re.sub()`を使います。この関数は、パターンを見つけてそれを新しい文字列に置き換えます。削除する場合は、新しい文字列として空文字（`''`）を指定します。

```Python
import re

# 見つけたいパターンを指定します。コメントは無視します。
text = 'Hello, World! ＃Comment'
pattern = r'＃.*'
clean_text = re.sub(pattern, '', text)
print(clean_text)
```

出力結果：
```
Hello, World! 
```

# 深掘り：

1. **歴史的な文脈：** 文字パターンの削除は古くからのテクニックで、UNIXのsedとawkというツールで初めて使用されました。Pythonでは、この機能を内蔵ライブラリ`re`で提供しています。
   
2. **代替手段：** `re.sub()`以外にも`str.replace()`を使う方法もありますが、こちらは正規表現を利用できないため、複雑なパターンの削除には向いていません。

3. **実装の詳細：** `re.sub()`は、最初に文中でパターンを検索し、その後該当部分を指定した文字列で置き換えます。パターンが見つからない場合、テキストはそのまま返されます。

# 参照：

- Python公式ドキュメンテーション：正規表現の操作 [https://docs.python.org/ja/3/library/re.html]()
- Regular Expression HOWTO [https://docs.python.org/ja/3/howto/regex.html]()
- Pythonの文字列操作のまとめ [https://note.nkmk.me/python-str-extract-substring/]()