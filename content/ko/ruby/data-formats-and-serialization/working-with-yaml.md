---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:56.916284-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Ruby\uC5D0\uB294 Psych\uB77C\uACE0 \uBD88\
  \uB9AC\uB294 \uB0B4\uC7A5 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uC788\uC5B4 YAML\uC744\
  \ \uD30C\uC2F1\uD558\uACE0 \uC0DD\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC774\
  \uB97C \uC0AC\uC6A9\uD558\uB824\uBA74 \uBA3C\uC800 YAML \uD45C\uC900 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uB97C \uC694\uAD6C\uD574\uC57C \uD569\uB2C8\uB2E4. \uC2DC\uC791\
  \uD558\uB294 \uB370 \uB3C4\uC6C0\uC774 \uB420 \uAE30\uBCF8 \uC608\uC81C\uC785\uB2C8\
  \uB2E4."
lastmod: '2024-03-13T22:44:56.025570-06:00'
model: gpt-4-0125-preview
summary: "Ruby\uC5D0\uB294 Psych\uB77C\uACE0 \uBD88\uB9AC\uB294 \uB0B4\uC7A5 \uB77C\
  \uC774\uBE0C\uB7EC\uB9AC\uAC00 \uC788\uC5B4 YAML\uC744 \uD30C\uC2F1\uD558\uACE0\
  \ \uC0DD\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

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
