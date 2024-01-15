---
title:                "yaml로 작업하기"
html_title:           "Java: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜

YAML을 사용하여 작업하는 이유는 매우 쉽습니다. YAML은 모든 언어에서 사용할 수 있으며, 구조적이고 읽기 쉬운 형식을 가지고 있어 데이터 관리에 유용합니다.

## 사용 방법

```Java
import org.yaml.snakeyaml.Yaml;

public class YAMLExample {
    public static void main(String[] args) {
        // YAML 파일에서 데이터 읽어오기
        Yaml yaml = new Yaml();
        Map<String, Object> data = yaml.load(YAMLExample.class.getResourceAsStream("data.yaml"));

        // 데이터 수정하기
        data.put("name", "John Doe");
        data.put("age", 25);

        // 수정된 데이터 YAML 파일로 저장하기
        try {
            yaml.dump(data, new FileWriter("new_data.yaml"));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

위 코드는 YAML 파일에서 데이터를 읽어와 수정하고, 수정된 데이터를 새로운 YAML 파일로 저장하는 간단한 예시입니다. 코드를 실행하면 다음과 같이 데이터가 수정되고 새로운 파일이 생성될 것입니다:

```yaml
name: John Doe
age: 25
```

## 깊이있는 살펴보기

YAML을 사용하면 데이터를 구조적으로 관리할 수 있으며, 코드의 가독성을 높일 수 있습니다. 또한, 변수와 인스턴스의 설정 파일로 사용할 수 있어 유용합니다. YAML 파싱 라이브러리를 이용하면, 복잡한 자료 구조도 쉽게 다룰 수 있습니다.

## 관련 링크

- [YAML 공식 사이트](https://yaml.org/)
- [SnakeYAML 라이브러리](https://bitbucket.org/asomov/snakeyaml/src/master/)
- [YAML 소개 및 사용법 블로그 포스트](https://medium.com/data-structures-and-algorithms-in-coding/yaml-%EC%86%8C%EA%B0%9C-%EB%B0%8F-%EC%82%AC%EC%9A%A9%EB%B2%95-cf2b9e456a86)