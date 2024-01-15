---
title:                "yaml로 작업하기"
html_title:           "Ruby: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜인가요?
이 글은 많은 프로그래밍 언어 중 하나인 루비(Ruby)에서 YAML을 다루는 방법을 알려드리고자 쓰여졌습니다. YAML은 데이터 시리얼라이제이션(Data Serialization)에 매우 유용한 포맷으로, 개발자들이 데이터를 보다 쉽게 저장하고 공유할 수 있도록 도와줍니다. YAML을 업무에 활용하면 보다 효율적인 데이터 관리가 가능합니다.

## 어떻게 하나요?
YAML을 다루는 방법은 매우 간단합니다. 아래의 예시 코드를 참고하면 쉽게 익힐 수 있습니다. 모든 코드는 루비 문법에 맞게 작성되었으며 실행 결과도 함께 제공합니다.
```Ruby
# YAML 라이브러리 불러오기
require 'yaml'

# YAML 형식의 데이터 생성
data = {name: "John", age: 25, hobbies: ["reading", "playing guitar"]}

# 데이터를 YAML 포맷으로 변환하기
yaml_data = YAML.dump(data)
#=> "---\n:name: John\n:age: 25\n:hobbies:\n- reading\n- playing guitar\n"

# YAML 형식의 데이터 읽어오기
new_data = YAML.load(yaml_data)

# 원하는 데이터 가져오기
puts new_data[:name]
#=> John
puts new_data[:age]
#=> 25
puts new_data[:hobbies][0]
#=> reading
```

## 깊게 들어가보기
YAML 형식의 데이터는 사람이 읽고 쓰기가 쉬우면서도 컴퓨터가 이해하기에도 쉽습니다. 이러한 특성 덕분에 YAML은 많은 프로그래밍 언어에서 널리 사용되며, 자유롭게 활용할 수 있습니다. 또한 YAML은 공백을 이용해 데이터를 구분하는 구조를 가지고 있어 구조적으로 보기 쉽습니다. 형식이 정해져 있지 않기 때문에 실수나 수정이 쉽고 유연하게 사용할 수 있습니다. YAML 공식 홈페이지에서 더 많은 정보를 확인할 수 있습니다.

## 더 자세한 정보는 아래 링크를 참고해주세요.
- YAML 공식 홈페이지: https://yaml.org/
- 루비 공식 홈페이지: https://www.ruby-lang.org/ko/
- YAML 라이브러리 문서: https://ruby-doc.org/stdlib-2.6.3/libdoc/yaml/rdoc/YAML.html