---
title:                "yaml과 작업하기"
html_title:           "Ruby: yaml과 작업하기"
simple_title:         "yaml과 작업하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## 무엇과 왜? 
YAML을 사용하는 것은 코드를 저장하고 전달하는 방식을 간소화하기 위해 프로그래머들이 사용하는 방법입니다. 이는 구조화된 데이터를 쉽게 읽거나 작성할 수 있도록 해주며, 인간이 쉽게 이해할 수 있는 형식으로 정보를 저장합니다.

## 방법: 
Ruby에서 YAML을 다루는 것은 매우 간단합니다. 다음 예제를 따라해보세요!

```Ruby
# YAML 파일을 읽어서 해시로 변환
hash = YAML.load_file("file.yml")

# 해시를 YAML 파일로 저장
File.open("file.yml", "w") do |f|
  f.write(hash.to_yaml)
end

# YAML 문자열을 해시로 변환
hash = YAML.load("---\n key: value \n other_key: other_value")

# 해시를 YAML 문자열로 변환
yaml_string = hash.to_yaml
```

## 더 깊게: 
YAML은 2001년 더그첸코(Danese Cooper)와 클락 에반스(Clark Evans)에 의해 처음으로 소개되었습니다. 이는 XML을 대체하기 위한 경량 마크업 언어로 개발되었으며, 간단하고 가독성이 좋은 문법을 가지고 있습니다. YAML에 대안으로는 JSON이 있지만, YAML이 더 자유로운 문법과 더 넓은 데이터 표현 범위를 가지고 있습니다. Ruby에서는 Psych 모듈을 통해 YAML을 지원합니다.

## 더 많은 정보를 알고 싶다면: 
- YAML 공식 문서: https://yaml.org/
- YAML 튜토리얼: https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/
- Psych 모듈 공식 문서: https://ruby-doc.org/stdlib-2.7.1/libdoc/psych/rdoc/Psych.html