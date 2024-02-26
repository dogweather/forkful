---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:00.019384-07:00
description: "Java\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\u306F\u3001\
  \u3055\u307E\u3056\u307E\u306A\u6761\u4EF6\u4E0B\u3067\u30B3\u30FC\u30C9\u304C\u671F\
  \u5F85\u901A\u308A\u306B\u52D5\u4F5C\u3059\u308B\u304B\u3092\u691C\u8A3C\u3059\u308B\
  \u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30D0\u30B0\u306E\u9632\u6B62\u3001\u5909\u66F4\u5F8C\u306E\u6A5F\
  \u80FD\u306E\u6B63\u78BA\u6027\u306E\u78BA\u4FDD\u3001\u826F\u597D\u306A\u30BD\u30D5\
  \u30C8\u30A6\u30A7\u30A2\u8A2D\u8A08\u539F\u5247\u306E\u4FC3\u9032\u306E\u305F\u3081\
  \u306B\u30C6\u30B9\u30C8\u3092\u66F8\u304D\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:39.983795-07:00'
model: gpt-4-0125-preview
summary: "Java\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\u306F\u3001\u3055\
  \u307E\u3056\u307E\u306A\u6761\u4EF6\u4E0B\u3067\u30B3\u30FC\u30C9\u304C\u671F\u5F85\
  \u901A\u308A\u306B\u52D5\u4F5C\u3059\u308B\u304B\u3092\u691C\u8A3C\u3059\u308B\u3053\
  \u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u30D0\u30B0\u306E\u9632\u6B62\u3001\u5909\u66F4\u5F8C\u306E\u6A5F\u80FD\
  \u306E\u6B63\u78BA\u6027\u306E\u78BA\u4FDD\u3001\u826F\u597D\u306A\u30BD\u30D5\u30C8\
  \u30A6\u30A7\u30A2\u8A2D\u8A08\u539F\u5247\u306E\u4FC3\u9032\u306E\u305F\u3081\u306B\
  \u30C6\u30B9\u30C8\u3092\u66F8\u304D\u307E\u3059\u3002"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ?
Javaでテストを書くことは、さまざまな条件下でコードが期待通りに動作するかを検証することについてです。プログラマーは、バグの防止、変更後の機能の正確性の確保、良好なソフトウェア設計原則の促進のためにテストを書きます。

## 方法:
Java開発者は、主にJUnitとTestNGという二つのテストフレームワークを使用します。ここでは、その使いやすさと幅広い採用により、テストの書き方においてより人気のある選択肢であるJUnitに焦点を当てます。

### JUnitの基本

MavenプロジェクトでJUnitを使うには、次の依存関係を`pom.xml`に追加します:

```xml
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <version>5.9.0</version>
    <scope>test</scope>
</dependency>
```

JUnitの基本的なテストは次のようになります:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 should equal 5");
    }
}
```

このテストを実行すると、`add`メソッドが期待通りに動作していれば合格となり、そうでなければエラーメッセージを表示して失敗となります。

### Mockitoを用いたモック

現実のシナリオでは、オブジェクトはしばしば他のオブジェクトに依存します。Mockitoは、テストのためにモックオブジェクトを作成するのに役立つ人気のあるモッキングフレームワークです。

MavenプロジェクトにMockitoを追加します:

```xml
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <version>4.5.1</version>
    <scope>test</scope>
</dependency>
```

Mockitoを使用した単純な使用例:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Test
    public void testGetUsername() {
        // UserRepositoryのモックを作成
        UserRepository mockRepository = mock(UserRepository.class);

        // モックオブジェクトの振る舞いを定義
        when(mockRepository.getUsername(1)).thenReturn("john_doe");

        UserService userService = new UserService(mockRepository);
        
        assertEquals("john_doe", userService.getUsername(1), "User ID 1 should be john_doe");
    }
}
```

このモックにより、実際の`UserRepository`がなくても`UserService`をテストすることができ、テストが`UserService`自体のロジックに集中できるようになります。
