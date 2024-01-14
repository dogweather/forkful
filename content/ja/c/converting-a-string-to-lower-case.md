---
title:                "C: 文字列を小文字に変換する"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換する理由は、データの整合性を保つためです。一般的に、プログラミングで文字列の検索や比較を行う場合、大文字と小文字が区別されるため、文字列をすべて小文字に統一することが必要になります。

## 方法

以下のコード例を参考に、文字列を小文字に変換する方法をご紹介します。

```C
// 文字列を小文字に変換する関数
void toLower(char* str)
{
    int i;
    for(i = 0; str[i] != '\0'; i++)
    {
        if(str[i] >= 'A' && str[i] <= 'Z') // 文字が大文字の場合
        {
            str[i] = str[i] + 32; // ASCIIコードで小文字に変換
        }
    }
}

int main()
{
    char str[] = "HELLO, WORLD!";
    
    printf("変換前: %s\n", str); // 変換前の文字列を出力
    toLower(str); // toLower関数を呼び出し、引数として文字列を渡す
    printf("変換後: %s\n", str); // 変換後の文字列を出力
    
    return 0;
}
```

上記のコードを実行すると、以下のような出力結果が得られます。

```
変換前: HELLO, WORLD!
変換後: hello, world!
```

このように、大文字が小文字に変換されたことが確認できます。

## ディープダイブ

文字列を小文字に変換する際には、文字コードの知識が必要になります。ASCIIコードでは、大文字と小文字の文字がそれぞれ異なるコードを持っています。そのため、単純に文字を『+32』することで小文字に変換することができるのです。

また、注意しなければならないのは、ASCIIコードにおいて大文字と小文字の文字の間には他の文字がいくつか存在することもあるということです。たとえば、'Z'と'['の間には、さらに'\', ']', '^'の4つの文字が存在します。このような場合、単純に『+32』することでは小文字に変換できないため、より複雑な処理が必要になります。

## 関連情報

- [ASCIIコードについて](https://bicycle1885.hatenablog.com/entry/20111029/1319856888)
- [文字列の比較について](https://programming-hobby.biz/archives/1414)
- [文字列検索について](https://wa3.i-3-i.info/word11846.html)

## 関連情報

- [ASCII code](https://www.ascii-code.com/)
- [String functions in C](https://www.geeksforgeeks.org/string-functions-in-c/)