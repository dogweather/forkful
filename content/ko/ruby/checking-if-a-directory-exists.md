---
title:                "디렉토리 존재 여부 확인하기"
date:                  2024-01-20T14:58:31.285525-07:00
html_title:           "Fish Shell: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 & 왜?)

디렉토리 존재 여부를 확인하는 것은 특정 경로에 폴더가 있는지 알아보는 과정입니다. 프로그래머들은 파일을 읽거나 쓰기 전에 에러를 방지하기 위해 이를 수행합니다.

## How to: (방법)

Ruby 코드는 간결합니다. `Dir.exist?` 메소드를 사용해 디렉토리의 존재 여부를 확인할 수 있습니다.

```Ruby
if Dir.exist?('some/directory')
  puts '디렉토리가 존재합니다!'
else
  puts '디렉토리가 존재하지 않습니다.'
end
```

샘플 출력은 다음과 같습니다.

```
디렉토리가 존재합니다!
```

또는

```
디렉토리가 존재하지 않습니다.
```

## Deep Dive (깊이 있는 분석)

이전 버전의 Ruby에서는 `File.exist?`를 사용해 디렉토리가 존재하는지 체크했습니다. 하지만 `Dir.exist?` 가 도입되면서 디렉토리 검사가 더 분명해졌습니다.

대안으로, 라이브러리나 젬 없이 순수 Ruby 코드만 사용할 때는 다른 메소드를 사용할 수도 있습니다. 예를 들어, `File.directory?` 메소드는 주어진 경로가 디렉토리인 경우에만 `true`를 반환합니다.

```Ruby
puts File.directory?('some/directory') ? '디렉토리입니다!' : '디렉토리가 아닙니다.'
```

구현 세부 사항에서는, `Dir.exist?` 메소드는 실제로 파일 시스템의 경로를 체크합니다. 경로가 존재하고 디렉토리인지 확인하면 Ruby 인터프리터는 이를 `true` 또는 `false`로 리턴합니다.

## See Also (참고 자료)

- Ruby 공식 문서 [`Dir.exist?`](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F)
- Ruby 공식 문서 [`File.directory?`](https://ruby-doc.org/core-2.7.1/File.html#method-c-directory-3F)
- Stack Overflow: [How to check if a directory exists in Ruby?](https://stackoverflow.com/questions/5477554/how-to-check-if-a-directory-exists-in-ruby)
