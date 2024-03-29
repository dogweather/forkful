---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:44.562413-07:00
description: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uB85C,\
  \ \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uAD6C\uC131 \uD30C\uC77C, \uB370\uC774\
  \uD130 \uB364\uD504, \uC5B8\uC5B4 \uAC04 \uB370\uC774\uD130 \uC804\uC1A1\uC744 \uC704\
  \uD574 \uC0AC\uC6A9\uD558\uB294 \uC0AC\uB78C\uC774 \uC77D\uC744 \uC218 \uC788\uB294\
  \ \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uD45C\uC900\uC785\uB2C8\uB2E4. \uADF8\uAC83\
  \uC740 \uAC00\uB3C5\uC131\uACFC \uC0AC\uC6A9\uC758 \uC6A9\uC774\uC131\uC73C\uB85C\
  \ \uC778\uAE30\uAC00 \uC788\uC73C\uBA70, \uC751\uC6A9 \uD504\uB85C\uADF8\uB7A8\uACFC\
  \ \uC11C\uBE44\uC2A4\uB97C\u2026"
lastmod: '2024-03-13T22:44:55.079374-06:00'
model: gpt-4-0125-preview
summary: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uB85C, \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uAD6C\uC131 \uD30C\uC77C, \uB370\uC774\uD130\
  \ \uB364\uD504, \uC5B8\uC5B4 \uAC04 \uB370\uC774\uD130 \uC804\uC1A1\uC744 \uC704\
  \uD574 \uC0AC\uC6A9\uD558\uB294 \uC0AC\uB78C\uC774 \uC77D\uC744 \uC218 \uC788\uB294\
  \ \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uD45C\uC900\uC785\uB2C8\uB2E4. \uADF8\uAC83\
  \uC740 \uAC00\uB3C5\uC131\uACFC \uC0AC\uC6A9\uC758 \uC6A9\uC774\uC131\uC73C\uB85C\
  \ \uC778\uAE30\uAC00 \uC788\uC73C\uBA70, \uC751\uC6A9 \uD504\uB85C\uADF8\uB7A8\uACFC\
  \ \uC11C\uBE44\uC2A4\uB97C\u2026"
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
YAML은 "YAML Ain't Markup Language"의 약자로, 프로그래머들이 구성 파일, 데이터 덤프, 언어 간 데이터 전송을 위해 사용하는 사람이 읽을 수 있는 데이터 직렬화 표준입니다. 그것은 가독성과 사용의 용이성으로 인기가 있으며, 응용 프로그램과 서비스를 구성하기 위한 일반적인 선택입니다.

## 방법:
Java에서는 Java 표준 에디션이 YAML에 대한 내장 지원을 포함하고 있지 않기 때문에, 서드 파티 라이브러리를 사용하여 YAML 파일을 작업할 수 있습니다. SnakeYAML은 파싱하고 YAML 데이터를 쉽게 생성할 수 있게 하는 인기 있는 라이브러리 중 하나입니다.

### SnakeYAML 설정
먼저 프로젝트에 SnakeYAML을 포함시킵니다. Maven을 사용하는 경우, 다음 의존성을 `pom.xml`에 추가하세요:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### YAML 읽기
```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class ReadYamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        try (InputStream inputStream = ReadYamlExample.class
                .getClassLoader()
                .getResourceAsStream("config.yml")) {
            Map<String, Object> data = yaml.load(inputStream);
            System.out.println(data);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
`config.yml`이 다음과 같다고 가정합니다:
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
출력은 다음과 같을 것입니다:
```
{name=Example, version=1.0, features=[login, signup]}
```

### YAML 작성
Java 객체에서 YAML을 생성하려면 SnakeYAML이 제공하는 `dump` 메소드를 사용하세요:
```java
import org.yaml.snakeyaml.Yaml;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

public class WriteYamlExample {
    public static void main(String[] args) {
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("name", "Example");
        data.put("version", 1.0);
        data.put("features", Arrays.asList("login", "signup"));

        Yaml yaml = new Yaml();
        String output = yaml.dump(data);
        System.out.println(output);
    }
}
```
이것은 다음과 같은 YAML 내용을 생성하고 인쇄할 것입니다:
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
SnakeYAML을 활용함으로써, 자바 개발자들은 응용 프로그램에 YAML 파싱과 생성을 쉽게 통합할 수 있으며, 구성 및 데이터 교환 목적을 위한 YAML의 가독성과 단순성의 혜택을 누릴 수 있습니다.
