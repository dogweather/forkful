---
title:                "JSON과 함께 작업하기"
html_title:           "Bash: JSON과 함께 작업하기"
simple_title:         "JSON과 함께 작업하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/working-with-json.md"
---

{{< edit_this_page >}}

# What & Why?
JSON은 프로그래머들이 데이터를 쉽게 교환하고 저장할 수 있게 도와주는 형식입니다. JSON은 프로그래밍 언어와 운영 체제에 독립적이기 때문에, 여러 언어나 운영 체제에서 일관성 있는 데이터 교환을 가능하게 합니다. 그래서, 많은 프로그래머들이 일상적인 작업에서 JSON을 사용하게 됩니다.

# How to:
```Bash
# JSON 파일 생성
echo '{"name": "John", "age": 30, "city": "New York"}' > person.json
# JSON 파일 읽기
name=$(jq '.name' person.json)
echo "The person's name is $name."
# JSON 배열 만들기
json_array=$(jq -ncM '[{"name": "John"}, {"name": "Jane"}]')
echo "The first person in the array is $(echo $json_array | jq '.[0].name')."
```

Output:
```
The person's name is John.
The first person in the array is John.
```

# Deep Dive:
JSON은 2002년 Douglas Crockford에 의해 만들어졌습니다. 그 이전에는 XML이 데이터 저장 포맷으로 많이 사용되었지만, XML은 파일의 크기와 복잡성 때문에 많은 프로그래머들에게 불편함을 초래했습니다. JSON은 간결하고 읽기 쉬운 포맷이기 때문에 프로그래머들 사이에서 빠르게 유행하게 되었습니다.

JSON은 다른 포맷으로 데이터를 저장할 수 있는 대안이 될 수 있습니다. 가장 대표적인 대안은 XML이지만, 다른 대안으로는 YAML, CSV, INI 파일 등이 있습니다. 하지만 이러한 대안들은 모두 JSON보다 복잡하고 덜 일반적입니다. 또한, JSON은 대부분의 프로그래밍 언어에서 기본으로 제공하기 때문에 추가 패키지를 설치할 필요가 없습니다.

JSON 데이터를 처리하기 위해서는 일반 텍스트 파일을 분석하여 원하는 정보를 추출하는 과정을 거쳐야 합니다. 이 작업을 위해 프로그램 언어에서 제공하는 JSON 파서를 사용하거나, 부가적인 패키지를 사용해야 할 수도 있습니다.

# See Also:
- JSON 공식 사이트: https://www.json.org/
- JSON 파서 라이브러리: https://www.json.org/json-ko.html