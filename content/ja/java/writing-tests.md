---
title:                "テストの作成"
date:                  2024-02-03T19:31:00.019384-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
