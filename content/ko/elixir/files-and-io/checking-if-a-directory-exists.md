---
title:                "디렉토리가 존재하는지 확인하기"
aliases: - /ko/elixir/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:08.350376-07:00
model:                 gpt-4-0125-preview
simple_title:         "디렉토리가 존재하는지 확인하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
Elixir에서 디렉터리의 존재 여부를 확인하는 것은 파일 시스템에서 지정된 경로에 디렉터리의 존재를 검증하는 것에 관한 것입니다. 프로그래머들은 이러한 작업을 통해 디렉터리가 존재하지 않는 것으로 인한 오류 없이 안전하게 읽기, 쓰기 또는 다른 작업을 수행할 수 있는지 확인합니다.

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
