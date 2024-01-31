---
title:                "YAML 다루기"
date:                  2024-01-19
simple_title:         "YAML 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
YAML은 데이터 직렬화 포맷입니다. 설정, 데이터 전송, 메타데이터 관리에 용이하기 때문에 프로그래머가 주로 사용합니다.

## How to: (방법)
Java에서 YAML 파일을 읽고 쓰기 위해 `SnakeYAML` 라이브러리를 사용합니다. 아래 예제와 같이 라이브러리를 추가하고 간단한 YAML 사용법을 확인할 수 있습니다.

```Java
// Maven dependency를 추가
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.29</version>
</dependency>

// 간단한 YAML 읽기 예제
import org.yaml.snakeyaml.Yaml;

import java.io.InputStream;
import java.util.Map;

public class YAMLExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        InputStream inputStream = YAMLExample.class
          .getClassLoader()
          .getResourceAsStream("test.yaml");
        
        Map<String, Object> data = yaml.load(inputStream);
        System.out.println(data);
    }
}
```
```test.yaml``` 파일에 다음 내용을 넣습니다.
```YAML
name: Yoon
age: 29
```
실행하면 다음과 같은 출력을 볼 수 있습니다.
```
{age=29, name=Yoon}
```

## Deep Dive (심층 분석)
YAML은 "YAML Ain't Markup Language"의 약자로, 2001년에 개발되었습니다. JSON과 비교하여 가독성이 높지만, 파싱은 덜 엄격합니다. Java에서는 SnakeYAML 라이브러리 외에도 `Jackson`, `org.yaml` 등 다른 라이브러리를 사용할 수 있습니다. 구현상의 특이사항으로는 탭 문자를 사용할 수 없고, 공백을 사용해야 한다는 점이 있습니다.

## See Also (참고자료)
SnakeYAML 공식 문서: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation
YAML 공식 사이트: https://yaml.org
Jackson YAML GitHub 페이지: https://github.com/FasterXML/jackson-dataformats-text/tree/master/yaml
