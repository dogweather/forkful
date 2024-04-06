---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:50.311133-07:00
description: "How to: C \u306F\u3001\u6587\u5B57\u5217\u4E0A\u3067\u76F4\u63A5\u691C\
  \u7D22\u3068\u7F6E\u63DB\u3092\u884C\u3046\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\
  \u95A2\u6570\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u305B\u3093\u3002\u3057\u304B\
  \u3057\u3001`<string.h>`\u2026"
lastmod: '2024-04-05T21:53:43.555916-06:00'
model: gpt-4-0125-preview
summary: "C \u306F\u3001\u6587\u5B57\u5217\u4E0A\u3067\u76F4\u63A5\u691C\u7D22\u3068\
  \u7F6E\u63DB\u3092\u884C\u3046\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u95A2\u6570\
  \u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u305B\u3093\u3002\u3057\u304B\u3057\u3001\
  `<string.h>` \u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u3042\u308B\u69D8\u3005\u306A\u6587\
  \u5B57\u5217\u51E6\u7406\u95A2\u6570\u3068\u3044\u304F\u3064\u304B\u306E\u30AB\u30B9\
  \u30BF\u30E0\u30ED\u30B8\u30C3\u30AF\u3092\u7D44\u307F\u5408\u308F\u305B\u308B\u3053\
  \u3068\u3067\u3001\u3053\u308C\u3092\u9054\u6210\u3059\u308B\u3053\u3068\u304C\u3067\
  \u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u6587\u5B57\u5217\u5185\u3067\u30B5\
  \u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u3092\u691C\u7D22\u3057\u7F6E\u63DB\u3059\u308B\
  \u57FA\u672C\u7684\u306A\u4F8B\u3067\u3059\u3002\u7C21\u7D20\u5316\u306E\u305F\u3081\
  \u3001\u3053\u306E\u4F8B\u306F\u5341\u5206\u306A\u30D0\u30C3\u30D5\u30A1\u30B5\u30A4\
  \u30BA\u3092\u4EEE\u5B9A\u3057\u3066\u304A\u308A\u3001\u672C\u756A\u30B3\u30FC\u30C9\
  \u3067\u306F\u8003\u616E\u3059\u3079\u304D\u30E1\u30E2\u30EA\u5272\u308A\u5F53\u3066\
  \u306E\u554F\u984C\u3092\u6271\u3063\u3066\u3044\u307E\u305B\u3093\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

## How to:
C は、文字列上で直接検索と置換を行うための組み込み関数を提供していません。しかし、`<string.h>` ライブラリにある様々な文字列処理関数といくつかのカスタムロジックを組み合わせることで、これを達成することができます。以下は、文字列内でサブストリングを検索し置換する基本的な例です。簡素化のため、この例は十分なバッファサイズを仮定しており、本番コードでは考慮すべきメモリ割り当ての問題を扱っていません。

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void replaceSubstring(char *source, char *sub, char *new_sub) {
    char buffer[1024];
    char *insert_point = &buffer[0];
    const char *tmp = source;
    size_t len_sub = strlen(sub), len_new_sub = strlen(new_sub);
    size_t len_up_to_match;

    while ((tmp = strstr(tmp, sub))) {
        // マッチまでの長さを計算
        len_up_to_match = tmp - source;
        
        // マッチ前の部分をコピー
        memcpy(insert_point, source, len_up_to_match);
        insert_point += len_up_to_match;
        
        // 新しいサブストリングをコピー
        memcpy(insert_point, new_sub, len_new_sub);
        insert_point += len_new_sub;
        
        // ソース文字列内のマッチを超えて移動
        tmp += len_sub;
        source = tmp;
    }
    
    // ソース文字列の残りの部分をコピー
    strcpy(insert_point, source);
    
    // 変更された文字列を出力
    printf("Modified string: %s\n", buffer);
}

int main() {
    char sourceStr[] = "Hello, this is a test. This test is simple.";
    char sub[] = "test";
    char newSub[] = "sample";
    
    replaceSubstring(sourceStr, sub, newSub);
    
    return 0;
}
```

サンプル出力：
```
Modified string: Hello, this is a sample. This sample is simple.
```

このコードは、ソース文字列内のサブストリング（`sub`）の全インスタンスを検索し、別のサブストリング（`newSub`）で置き換える簡単な方法を示しています。各マッチの開始点を見つけるために `strstr` 関数を使用します。これは重複するサブストリングなどの複雑なシナリオを扱わない非常に基本的な例です。

## Deep Dive
"How to" セクションで使用されたアプローチは基本的で、第三者のライブラリーなしで C におけるテキスト検索と置換を実現する方法を示しています。C は低レベルのメモリ管理と性能に重点を置いているため、標準ライブラリは Python や JavaScript のような言語に見られる高レベルの文字列操作機能をカプセル化していません。プログラマーは、手動でメモリを管理し、さまざまな文字操作を組み合わせて目的の結果を達成する必要があり、これは複雑さを増すが、より多くの制御と効率性を提供します。

この手動アプローチは、特にメモリ割り当てやバッファサイズの管理において、エラーが発生しやすいことに注意することが重要です。不正確な処理はバッファオーバーフローやメモリ破損につながり、コードをセキュリティリスクにさらす可能性があります。

特に複雑なテキスト処理が必要な実用的なシナリオでは、PCRE（Perl Compatible Regular Expressions）のような正規表現ベースの検索と置換を用いる第三者のライブラリを統合することを検討する価値がよくあります。これはコードを簡素化し、エラーの可能性を減らすことができます。さらに、現代のC標準とコンパイラは、より安全な文字列操作のための組み込み関数や代替手段をますます提供しており、古いCコードベースで観察される一般的な落とし穴を軽減することを目指しています。しかし、特に性能が重要なアプリケーションを最適化する際には、手動でのテキスト処理の基本的な理解が、プログラマーのツールボックスの中で貴重なスキルとして残ります。
