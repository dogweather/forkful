---
aliases:
- /ko/ruby/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:56.916284-07:00
description: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uB85C,\
  \ \uC778\uAC04\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uD615\uC2DD\uC73C\uB85C \uB418\
  \uC5B4 \uC788\uC5B4 Ruby\uC5D0\uC11C \uC124\uC815 \uD30C\uC77C \uBC0F \uB370\uC774\
  \uD130 \uC9C1\uB82C\uD654\uC5D0 \uAD11\uBC94\uC704\uD558\uAC8C \uC0AC\uC6A9\uB429\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uAC1D\
  \uCCB4\uB97C \uC77D\uAE30 \uC27D\uACE0 \uAD6C\uC870\uD654\uB41C \uBC29\uC2DD\uC73C\
  \uB85C \uC800\uC7A5\uD558\uAC70\uB098 \uC804\uB2EC\uD574\uC57C \uD560 \uB54C YAML\uC744\
  \u2026"
lastmod: 2024-02-18 23:09:07.057440
model: gpt-4-0125-preview
summary: "YAML\uC740 \"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uB85C, \uC778\
  \uAC04\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uD615\uC2DD\uC73C\uB85C \uB418\uC5B4\
  \ \uC788\uC5B4 Ruby\uC5D0\uC11C \uC124\uC815 \uD30C\uC77C \uBC0F \uB370\uC774\uD130\
  \ \uC9C1\uB82C\uD654\uC5D0 \uAD11\uBC94\uC704\uD558\uAC8C \uC0AC\uC6A9\uB429\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uAC1D\uCCB4\
  \uB97C \uC77D\uAE30 \uC27D\uACE0 \uAD6C\uC870\uD654\uB41C \uBC29\uC2DD\uC73C\uB85C\
  \ \uC800\uC7A5\uD558\uAC70\uB098 \uC804\uB2EC\uD574\uC57C \uD560 \uB54C YAML\uC744\
  \u2026"
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
YAML은 "YAML Ain't Markup Language"의 약자로, 인간이 읽을 수 있는 형식으로 되어 있어 Ruby에서 설정 파일 및 데이터 직렬화에 광범위하게 사용됩니다. 프로그래머들은 데이터 객체를 읽기 쉽고 구조화된 방식으로 저장하거나 전달해야 할 때 YAML을 선호하는데, 이는 구성 관리, 데이터 저장, 언어 간 데이터 공유 같은 작업을 단순화합니다.

## 사용 방법:
Ruby에는 Psych라고 불리는 내장 라이브러리가 있어 YAML을 파싱하고 생성할 수 있습니다. 이를 사용하려면 먼저 YAML 표준 라이브러리를 요구해야 합니다. 시작하는 데 도움이 될 기본 예제입니다:

```ruby
require 'yaml'

# 직렬화될 해시
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# 해시를 YAML로 변환
yaml_data = person.to_yaml

puts yaml_data
```

**샘플 출력:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

YAML 데이터를 Ruby 객체로 다시 로드하는 방법:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**샘플 출력:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### 서드파티 라이브러리 사용하기:

표준 라이브러리가 기본적인 작업에 충분하지만, 복잡한 필요성에 대해선 'safe_yaml' 같은 서드파티 젬을 살펴볼 수 있습니다. 이러한 라이브러리를 사용하기 위해서는 먼저 젬을 설치해야 합니다:

```bash
gem install safe_yaml
```

그런 다음, 사용자가 제어할 수 있는 소스로부터 객체 인스턴스화와 같은 위험을 완화하면서 YAML 데이터를 안전하게 로드하는 데 사용할 수 있습니다:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**샘플 출력:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

이 접근 방식은 YAML 처리의 보안을 강화하여, 신뢰할 수 없는 출처로부터 YAML을 로드하는 애플리케이션에 적합한 선택이 됩니다.
