---
title:                "YAML 다루기"
date:                  2024-01-19
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜 사용하는가?)
YAML은 "YAML Ain't Markup Language"의 약자로, 간결하고 사람이 읽기 쉬운 데이터 직렬화 포맷입니다. 설정 파일, 데이터 저장, 메타데이터 등의 경우 Ruby 프로그래머들이 YAML을 사용하여 복잡하지 않게 데이터를 주고받습니다.

## How to: (어떻게 사용하는가?)
```Ruby
require 'yaml'

# YAML 데이터를 Ruby 객체로 로드하기 
yaml_content = "
user:
  name: John Doe
  age: 34
  languages:
    - Ruby
    - Python
    - JavaScript
"

parsed_data = YAML.load(yaml_content)
puts parsed_data['user']['name']  # 출력: John Doe

# Ruby 객체를 YAML로 덤프(변환)하기
user_data = {
  user: {
    name: 'John Doe',
    age: 34,
    languages: ['Ruby', 'Python', 'JavaScript']
  }
}

yaml_data = user_data.to_yaml
puts yaml_data
```
## Deep Dive (심층 탐구)
처음에 YAML은 XML이 너무 복잡하다는 생각에서 태어났습니다. 2001년에 등장한 이후 데이터 표현을 위한 간단한 텍스트 기반의 포맷으로 널리 퍼졌어요. 대안으로는 JSON이나 XML이 있지만, YAML은 개행과 들여쓰기를 사용하여 가독성이 높다는 장점이 있죠. Ruby에서 YAML 처리는 'Psych' 라이브러리가 처리하며, 이는 Ruby의 표준 라이브러리에 포함되어 있습니다.

## See Also (더 보기)
- Official YAML Website: [https://yaml.org](https://yaml.org)
- YAML Syntax Quick Reference: [https://learnxinyminutes.com/docs/yaml/](https://learnxinyminutes.com/docs/yaml/)
