---
title:                "yaml로 작업하기"
html_title:           "Python: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/working-with-yaml.md"
---

{{< edit_this_page >}}

# 무엇과 왜?

YAML은 데이터를 저장하고 전송하기 위해 사용되는 파일 형식입니다. 프로그래머들은 YAML을 사용하여 파일을 더 읽기 쉽고 구조화할 수 있으며 다양한 언어에서 호환성을 보장할 수 있기 때문에 많이 사용합니다.

## 어떻게 하나요?

YAML 파일을 만들기 위해서는 먼저 `yaml` 모듈을 임포트해야 합니다. 다음으로 파일을 연 후, `yaml.dump()` 함수를 사용하여 데이터를 YAML 형식으로 저장할 수 있습니다. 예시 코드는 다음과 같습니다.

```python
import yaml

data = {
    'name': 'John',
    'age': 30
}

with open('data.yaml', 'w') as f:
    yaml.dump(data, f)
```

이 코드를 실행하면 `data.yaml` 파일에 다음과 같은 내용이 저장됩니다.

```yaml
name: John
age: 30
```

## 깊게 파헤쳐보기

YAML은 2001년에 Java 개발자인 Clark Evans가 개발한 형식입니다. YAML과 비슷한 다른 형식으로는 JSON이 있지만, YAML은 더 많은 기능과 유연성을 제공합니다. 또한, YAML은 다양한 언어에서 호환성을 보장하기 때문에 많은 프로그래머들에게 사랑받고 있습니다.

## 관련 자료

더 많은 정보를 원하시면 YAML 공식 문서나 유용한 YAML 자료를 참고하시기 바랍니다.

- YAML 공식 사이트: https://yaml.org/
- YAML 설명서: https://docs.python.org/ko/3/library/yaml.html