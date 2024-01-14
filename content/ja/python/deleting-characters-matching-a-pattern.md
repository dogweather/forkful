---
title:                "Python: パターンに一致する文字を削除する"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ？

文字マッチングを行うことで、特定の文字パターンに合致した文字を削除することができます。例えば、あなたのデータから不要な文字を取り除き、より整ったデータを得ることができます。

## 方法

```Python
# 文字マッチング関数を定義する
def delete_characters(text, pattern):
    # 文字列中のパターンに合致する文字を空文字列に置換する
    new_text = text.replace(pattern, "")
    # 結果をプリントアウトする
    print(new_text)

# 関数を実行する
delete_characters("Hello, World!", "l")
```

このコードの出力は次のようになります。

```
Heo, Word!
```

## ディープダイブ

この方法では、replace()メソッドを使用して文字列中の特定の文字を置換することで、文字マッチングを実現しています。このメソッドは、文字列の中で最初に見つかったパターンに合致する文字を指定した文字列に置き換えるものです。

また、文字マッチングには正規表現を用いることも可能です。この場合、複雑なパターンにも対応することができますが、正規表現の知識が必要になります。

## See Also

- [Pythonで文字列のマッチングを行う方法](https://note.nkmk.me/python-re-match-search-findall-etc/)
- [正規表現の基礎知識](https://docs.python.org/ja/3/howto/regex.html)