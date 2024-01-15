---
title:                "「文字列を小文字に変換する」"
html_title:           "C: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換することに興味がある理由は、文字列の大文字や小文字を区別しない検索や比較を行う際に役立つからです。また、データベースやAPIなどで取得した文字列を統一的な形式で扱うためにも必要です。

## 方法

よく使われる方法は文字列の各文字をループで取り出し、ASCIIコードを利用して大文字を小文字に変換する方法です。以下のように実装できます。

```C
// 大文字から小文字への変換関数
char toLower(char c){
    if(c >= 'A' && c <= 'Z'){
        return c + 'a' - 'A';
    }
    else{
        return c;
    }
}

// 文字列を小文字に変換する関数
void toLowerCase(char *str){
    for(int i=0; str[i]!='\0'; i++){
        str[i] = toLower(str[i]);
    }
}

// メイン関数で利用例
int main()
{
    char str[] = "HeLlO World";
    
    printf("変換前の文字列：%s\n", str);
    
    toLowerCase(str);
    
    printf("変換後の文字列：%s\n", str);
    
    return 0;
}

/*
出力結果：
変換前の文字列：HeLlO World
変換後の文字列：hello world
*/
```

## 深堀り

文字列を小文字に変換する際、注意すべき点がいくつかあります。例えば、日本語や多言語の文字はASCIIコードを利用して変換できないため、別の方法を用いる必要があります。また、大文字の文字列を小文字に変換するときに英語以外の言語で使用すると、アルファベット以外の文字がエラーを引き起こす場合があります。さらに、多言語対応のプログラムを作る場合にはロケール（言語と国の設定）を適切に設定することも重要です。

## 参考文献

- [C言語で文字列を小文字に変換する方法](https://www.techscore.com/blog/2017/11/09/c%E8%A8%80%E8%AA%9E%E3%81%A7%E6%96%87%E5%AD%97%E5%88%97%E3%82%92%E5%B0%8F%E6%96%87%E5%AD%97%E3%81%AB%E5%A4%89%E6%8F%9B%E3%81%99%E3%82%8B%E6%96%B9%E6%B3%95/)
- [C言語 小文字大文字の変換 ASCIIコード](https://qiita.com/ichironagai/items/7915a804f44106471f8b)