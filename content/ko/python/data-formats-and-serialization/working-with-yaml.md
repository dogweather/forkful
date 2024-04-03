---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:41.541287-07:00
description: "YAML\uC740 \"YAML Ain't Markup Language\"\uB97C \uC758\uBBF8\uD558\uBA70\
  , \uC778\uAC04\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uB370\uC774\uD130 \uC9C1\uB82C\
  \uD654 \uD3EC\uB9F7\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ YAML\uC744 \uC124\uC815 \uD30C\uC77C, \uD504\uB85C\uC138\uC2A4 \uAC04 \uBA54\uC2DC\
  \uC9D5, \uB370\uC774\uD130 \uC800\uC7A5\uC5D0 \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC774\
  \uB294 XML\uC774\uB098 JSON\uAC19\uC740 \uB2E4\uB978 \uD3EC\uB9F7\uB4E4\uC5D0 \uBE44\
  \uD574 \uBB38\uBC95\uC774 \uB2E8\uC21C\uD558\uACE0\u2026"
lastmod: '2024-03-13T22:44:54.628978-06:00'
model: gpt-4-0125-preview
summary: "YAML\uC740 \"YAML Ain't Markup Language\"\uB97C \uC758\uBBF8\uD558\uBA70\
  , \uC778\uAC04\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uB370\uC774\uD130 \uC9C1\uB82C\
  \uD654 \uD3EC\uB9F7\uC785\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

## 방법:
Python에서 YAML을 읽고 쓰기 위해서는 일반적으로 제3의 라이브러리를 사용하며, `PyYAML`이 가장 인기가 많습니다. 시작하기 위해서는 `pip install PyYAML`을 실행하여 PyYAML을 설치해야 합니다.

**예시: YAML 파일 쓰기**

```python
import yaml

data = {'a list': [1, 42, 3.141, 1337, 'help', u'€'],
        'a string': 'boo!',
        'another dict': {'foo': 'bar', 'key': 'value', 'the answer': 42}}

with open('example.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# 이것은 데이터를 YAML 포맷으로 구조화하여 `example.yaml`을 생성합니다.
```

**예시: YAML 파일에서 읽기**

```python
import yaml

with open('example.yaml', 'r') as f:
    data_loaded = yaml.safe_load(f)

print(data_loaded)

# 출력: 
# {'a list': [1, 42, 3.141, 1337, 'help', '€'],
#  'a string': 'boo!',
#  'another dict': {'foo': 'bar', 'key': 'value', 'the answer': 42}}
```

**구성을 위한 YAML 사용하기**

많은 프로그래머들이 응용 프로그램 구성을 관리하기 위해 YAML을 사용합니다. 다음은 구성 파일을 구조화하고 읽는 방법의 예입니다:

config.yaml:
```yaml
database:
  host: localhost
  port: 5432
  username: admin
  password: secret
```

Python에서 구성 파일 읽기:
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['database']['host'])  # 출력: localhost
```

**복잡한 구조 다루기**

PyYAML을 사용하여 복잡한 구조에 대해서는 사용자 정의 Python 객체를 정의할 수 있습니다. 그러나 임의의 함수나 객체를 실행시키지 않도록 `safe_load`를 사용하여 안전한 관행을 확실히 해야 합니다.

```python
import yaml

# Python 객체 정의하기
class Example:
    def __init__(self, value):
        self.value = value

# 사용자 정의 생성자
def constructor_example(loader, node):
    value = loader.construct_scalar(node)
    return Example(value)

# 태그 "!example"에 대한 생성자 추가
yaml.add_constructor('!example', constructor_example)

yaml_str = "!example 'data'"
loaded = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(loaded.value)  # 출력: data
```

이 코드 조각에서, `!example`은 YAML 문자열에서 'data' 값으로 `Example` 객체를 인스턴스화하기 위해 사용된 사용자 정의 태그입니다. 이런 사용자 정의 로더는 PyYAML의 유연성을 확장하여 더 복잡한 데이터 구조와 유형을 처리할 수 있게 합니다.
