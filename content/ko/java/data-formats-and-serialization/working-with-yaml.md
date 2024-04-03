---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:44.562413-07:00
description: "\uBC29\uBC95: Java\uC5D0\uC11C\uB294 Java \uD45C\uC900 \uC5D0\uB514\uC158\
  \uC774 YAML\uC5D0 \uB300\uD55C \uB0B4\uC7A5 \uC9C0\uC6D0\uC744 \uD3EC\uD568\uD558\
  \uACE0 \uC788\uC9C0 \uC54A\uAE30 \uB54C\uBB38\uC5D0, \uC11C\uB4DC \uD30C\uD2F0 \uB77C\
  \uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uC5EC YAML \uD30C\uC77C\uC744\
  \ \uC791\uC5C5\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. SnakeYAML\uC740 \uD30C\uC2F1\
  \uD558\uACE0 YAML \uB370\uC774\uD130\uB97C \uC27D\uAC8C \uC0DD\uC131\uD560 \uC218\
  \ \uC788\uAC8C \uD558\uB294 \uC778\uAE30 \uC788\uB294 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \ \uC911\u2026"
lastmod: '2024-03-13T22:44:55.079374-06:00'
model: gpt-4-0125-preview
summary: "Java\uC5D0\uC11C\uB294 Java \uD45C\uC900 \uC5D0\uB514\uC158\uC774 YAML\uC5D0\
  \ \uB300\uD55C \uB0B4\uC7A5 \uC9C0\uC6D0\uC744 \uD3EC\uD568\uD558\uACE0 \uC788\uC9C0\
  \ \uC54A\uAE30 \uB54C\uBB38\uC5D0, \uC11C\uB4DC \uD30C\uD2F0 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB97C \uC0AC\uC6A9\uD558\uC5EC YAML \uD30C\uC77C\uC744 \uC791\uC5C5\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

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
