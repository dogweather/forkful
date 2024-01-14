---
title:                "Ruby: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜
Ruby 프로그래밍을 배우면서 YAML을 동작하는 방법을 배우는 것은 매우 중요합니다. YAML은 데이터를 구조화하고 효율적으로 다룰 수 있는 간단하고 유연한 형식입니다. 또한 Ruby에서 YAML을 사용하기위한 내장 라이브러리가 있기 때문에 프로그램에 쉽게 적용할 수 있습니다.

## 방법
다음은 Ruby에서 YAML을 사용하는 간단한 예제입니다.

```Ruby
require 'yaml'

# YAML 파일 불러오기
data = YAML.load_file('my_file.yml')

# 데이터 구조 출력
puts data

# 데이터 수정
data['name'] = 'John Doe'

# 수정된 데이터를 새로운 YAML 파일로 저장
File.open('new_file.yml', 'w') do |file|
  YAML.dump(data, file)
end
```

위 예제에서는 YAML 파일을 불러오고, 데이터 구조를 출력하고, 데이터를 수정하여 새로운 파일로 저장하는 방법을 보여줍니다. 이 외에도 YAML을 사용하여 배열이나 해시와 같은 다양한 자료형을 다룰 수 있습니다.

## 딥 다이브
YAML은 일반적인 데이터 저장 형식인 JSON보다 더 유연하고 읽기 쉽습니다. YAML 파일은 들여쓰기를 사용하여 데이터의 구조를 정의하기 때문에 가독성이 뛰어나며, 주석을 사용하여 데이터에 대한 설명을 추가할 수도 있습니다. 또한 YAML은 여러 언어에서 호환성이 높아 데이터를 다양한 환경에서 쉽게 공유할 수 있습니다.

## 관련 자료
- https://docs.ruby-lang.org/en/2.6.0/YAML.html
- https://en.wikipedia.org/wiki/YAML
- http://www.yaml.org/
- https://www.rubyguides.com/2019/05/load-yaml-ruby/