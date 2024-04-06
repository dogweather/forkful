---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:00.019384-07:00
description: "\u65B9\u6CD5: Java\u958B\u767A\u8005\u306F\u3001\u4E3B\u306BJUnit\u3068\
  TestNG\u3068\u3044\u3046\u4E8C\u3064\u306E\u30C6\u30B9\u30C8\u30D5\u30EC\u30FC\u30E0\
  \u30EF\u30FC\u30AF\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u3053\u3053\u3067\u306F\
  \u3001\u305D\u306E\u4F7F\u3044\u3084\u3059\u3055\u3068\u5E45\u5E83\u3044\u63A1\u7528\
  \u306B\u3088\u308A\u3001\u30C6\u30B9\u30C8\u306E\u66F8\u304D\u65B9\u306B\u304A\u3044\
  \u3066\u3088\u308A\u4EBA\u6C17\u306E\u3042\u308B\u9078\u629E\u80A2\u3067\u3042\u308B\
  JUnit\u306B\u7126\u70B9\u3092\u5F53\u3066\u307E\u3059\u3002\u2026"
lastmod: '2024-03-13T22:44:41.952476-06:00'
model: gpt-4-0125-preview
summary: "Java\u958B\u767A\u8005\u306F\u3001\u4E3B\u306BJUnit\u3068TestNG\u3068\u3044\
  \u3046\u4E8C\u3064\u306E\u30C6\u30B9\u30C8\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\
  \u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u305D\u306E\
  \u4F7F\u3044\u3084\u3059\u3055\u3068\u5E45\u5E83\u3044\u63A1\u7528\u306B\u3088\u308A\
  \u3001\u30C6\u30B9\u30C8\u306E\u66F8\u304D\u65B9\u306B\u304A\u3044\u3066\u3088\u308A\
  \u4EBA\u6C17\u306E\u3042\u308B\u9078\u629E\u80A2\u3067\u3042\u308BJUnit\u306B\u7126\
  \u70B9\u3092\u5F53\u3066\u307E\u3059."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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
