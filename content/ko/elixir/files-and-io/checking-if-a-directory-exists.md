---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:08.350376-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694: Elixir\uC758 \uD45C\uC900 \uB77C\
  \uC774\uBE0C\uB7EC\uB9AC\uB294 `File` \uBAA8\uB4C8\uC744 \uD1B5\uD574 \uB514\uB809\
  \uD130\uB9AC\uC758 \uC874\uC7AC \uC5EC\uBD80\uB97C \uD655\uC778\uD558\uB294 \uAC04\
  \uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uB2E4\uC74C\uC740\
  \ \uADF8 \uC0AC\uC6A9 \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.743053-06:00'
model: gpt-4-0125-preview
summary: "Elixir\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB294 `File` \uBAA8\
  \uB4C8\uC744 \uD1B5\uD574 \uB514\uB809\uD130\uB9AC\uC758 \uC874\uC7AC \uC5EC\uBD80\
  \uB97C \uD655\uC778\uD558\uB294 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\
  \uD569\uB2C8\uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

## 어떻게 하나요:
Elixir의 표준 라이브러리는 `File` 모듈을 통해 디렉터리의 존재 여부를 확인하는 간단한 방법을 제공합니다. 다음은 그 사용 방법입니다:

```elixir
if File.dir?("path/to/directory") do
  IO.puts "디렉터리가 존재합니다!"
else
  IO.puts "디렉터리가 존재하지 않습니다."
end
```

디렉터리가 존재하지 않는다고 가정할 때의 샘플 출력:
```
디렉터리가 존재하지 않습니다.
```

디렉터리 존재 여부 검사를 포함한 보다 고급 파일 시스템 상호작용을 위해, `FileSystem`과 같은 타사 라이브러리를 고려할 수 있습니다. `FileSystem`은 복잡한 애플리케이션에 대해 더 세밀한 제어와 피드백을 제공할 수 있지만, 디렉터리가 존재하는지 여부를 확인하는 기본적인 필요성에 대해서는, 즉시 사용 가능하고 외부 의존성이 필요 없기 때문에 네이티브 `File` 모듈을 사용하는 것이 일반적으로 권장됩니다.
