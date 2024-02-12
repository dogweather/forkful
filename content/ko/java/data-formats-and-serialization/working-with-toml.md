---
title:                "프로그래머를 위한 TOML 다루기"
aliases:
- /ko/java/working-with-toml.md
date:                  2024-01-26T04:23:04.323515-07:00
model:                 gpt-4-0125-preview
simple_title:         "프로그래머를 위한 TOML 다루기"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/working-with-toml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
TOML은 Tom's Obvious, Minimal Language를 의미합니다. 이것은 구성 파일을 위해 사용되는 데이터 직렬화 형식입니다. 프로그래머들은 이를 쉽게 읽고 쓸 수 있으며, 해시 테이블에 잘 맵핑되기 때문에 사용합니다.

## 방법:
TOML 파싱 라이브러리가 필요합니다. `toml4j`를 추천합니다. 다음과 같이 프로젝트에 추가하세요:

```java
// 이것을 build.gradle에 추가하세요
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

TOML 파일을 파싱하는 방법은 다음과 같습니다:

```java
import com.moandjiezana.toml.Toml;

public class TomlExample {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("서버 IP: " + ip);
        System.out.println("서버 포트: " + port);
    }
}
```

샘플 출력:

```
서버 IP: 192.168.1.1
서버 포트: 80
```

## 심층 탐구
GitHub 공동 창업자인 Tom Preston-Werner가 개발한 TOML은 XML보다 간단하고 YAML보다 더 명세화되어 있을 것을 목표로 했습니다. 2021년에 출시된 최신 버전 1.0.0은 안정적인 기능 세트를 제공합니다.

JSON이나 YAML 같은 대안들도 인기가 있습니다. JSON은 데이터 교환이 좋고, YAML은 복잡한 구성에서 더 인간이 읽기 쉽습니다. TOML의 강점은 Rust 커뮤니티에서의 사용과 그것의 직관성입니다.

구현과 관련하여 Java와 함께 TOML을 사용할 때, 선택하는 파서가 중요하다는 것을 기억하세요. `toml4j`를 넘어, 일부는 `jackson-dataformat-toml`을 선택합니다. 이들 각각은 오류 처리나 파싱 성능 같은 뉘앙스를 가지므로, 프로젝트의 필요에 따라 선택하세요.

## 참고
- TOML 명세: https://toml.io/en/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
