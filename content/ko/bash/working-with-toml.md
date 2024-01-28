---
title:                "프로그래머를 위한 TOML 다루기"
date:                  2024-01-26T04:19:20.101511-07:00
model:                 gpt-4-0125-preview
simple_title:         "프로그래머를 위한 TOML 다루기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/working-with-toml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
TOML은 Tom's Obvious, Minimal Language의 약자로, 데이터 직렬화 형식입니다. 프로그래머들은 그것의 단순함과 가독성 때문에 선호하는데; 설정 파일에 아주 적합하며, YAML과 비슷한 느낌이지만 사람이 보기에는 JSON보다 덜 번거롭습니다.

## 방법:
먼저, Bash에서 TOML을 가지고 놀기 위해 `toml-cli`를 설치합니다. 즉석에서 TOML 파일을 읽거나 편집하기에 유용합니다.

```Bash
# toml-cli 설치, 우리의 TOML 작업을 위한 작은 도우미
pip install toml-cli

# 'config.toml'이라는 TOML 파일이 있다고 상상하세요
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# 값을 읽기
toml get config.toml owner.name
# 출력: Tom

# 값을 설정하기
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# 프로 팁: 점이 있거나 특이한 문자가 있는 키에는 따옴표를 사용하세요!
```

## 심층적 이해
사람들에게 있어 JSON의 장애물에 대한 불호에서 탄생한 TOML은 2013년경에 등장했습니다. GitHub의 공동 창립자인 Tom Preston-Werner는 무엇인가 아주 명확한 것을 원했습니다. YAML과 INI는 대안이었지만, TOML은 둘의 최고의 특성을 결합한 것 같습니다.

쉬뱅, 중첩된 데이터와 배열을 얻을 수 있으며, YAML의 발목 잡는 문제와 JSON의 중괄호 없이 말이죠. TOML은 이제 Rust의 Cargo에서 설정을 위한 가도의 선택이 되었으며, 이는 개발 세계에서의 그것의 인기를 말해줍니다. 이는 규격에 의해 주도되어, 모든 것을 타이트하고 잘 정의된 상태로 유지합니다. 거의 모든 언어에서 파서를 찾을 수 있어, 널리 채택될 수 있습니다.

## 참조
- 공식 TOML GitHub 저장소: https://github.com/toml-lang/toml
- PyPI의 toml-cli: https://pypi.org/project/toml-cli/
- 데이터 직렬화 형식의 비교: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
