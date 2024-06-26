---
date: 2024-01-26 04:26:02.160300-07:00
description: "\uBC29\uBC95: \uBA3C\uC800, `toml-rb` \uC82C\uC744 \uC124\uCE58\uD558\
  \uC138\uC694. \uC774\uB294 Ruby\uC5D0\uC11C TOML \uD30C\uC2F1\uC744 \uC704\uD55C\
  \ \uC778\uAE30 \uC788\uB294 \uC120\uD0DD\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:56.030024-06:00'
model: gpt-4-0125-preview
summary: "\uBA3C\uC800, `toml-rb` \uC82C\uC744 \uC124\uCE58\uD558\uC138\uC694."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
weight: 39
---

## 방법:
먼저, `toml-rb` 젬을 설치하세요. 이는 Ruby에서 TOML 파싱을 위한 인기 있는 선택입니다.

```Ruby
gem install toml-rb
```

다음으로, TOML 파일을 읽는 방법:

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

샘플 출력은 다음과 같을 수 있습니다:

```
My Awesome App
```

TOML 파일에 쓰기:

```Ruby
require 'toml-rb'

config = {
  'title' => 'My Awesome App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

`config.toml`을 확인하면 설정이 깔끔하게 저장된 것을 볼 수 있습니다.

## 심층 분석
TOML은 Tom Preston-Werner, GitHub의 공동 창립자에 의해 2013년경에 만들어진 것으로, Tom's Obvious, Minimal Language의 약자입니다. 이의 주요 목표는 데이터 구조로 쉽게 파싱할 수 있는 간단명료한 형식을 제공하는 것입니다. JSON은 API용으로 훌륭하고, YAML은 유연합니다만, TOML의 틈새는 인간 친화적이라는 강조점에 있습니다. 들여쓰기와 세밀한 부분에서 까다로울 수 있는 YAML과 달리, TOML은 많은 사람들이 더 단순하고 오류 발생률이 낮은 것으로 여기는 INI와 유사한 구조를 지향합니다.

JSON, YAML, XML 같은 대체재들은 각각의 장점을 가지고 있지만, TOML은 구성을 인간과 프로그램 모두 쉽게 유지할 수 있는 시나리오에서 빛을 발합니다. 단순함을 넘어서 엄격하고 읽기 쉬운 포매팅을 강제합니다.

기술적 측면에서, Ruby로 TOML 내용을 파싱하기 위해, 우리는 `toml-rb` 같은 젬을 활용합니다. 이 젬은 Ruby의 동적 성격을 활용하여, TOML 데이터를 네이티브 Ruby 해쉬, 배열, 그 밖의 기본 데이터 구조로 변환합니다. 이러한 변환은 개발자들이 익숙한 Ruby 의미론과 메소드를 사용하여 TOML 데이터를 가지고 작업할 수 있게 해줍니다.

## 관련 링크
- TOML 프로젝트 및 명세: https://toml.io/en/
- `toml-rb` 젬: https://github.com/emancu/toml-rb
- TOML, YAML, JSON 비교: https://blog.theodo.com/2021/08/compare-yml-toml-json/
