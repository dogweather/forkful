---
title:                "디렉토리가 존재하는지 확인하기"
aliases:
- ko/ruby/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:23.528480-07:00
model:                 gpt-4-0125-preview
simple_title:         "디렉토리가 존재하는지 확인하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
루비에서 디렉토리의 존재 여부를 확인하는 것은 프로그래머가 파일을 읽거나 새 디렉토리를 생성하기 전에 디렉토리의 존재를 검증할 수 있도록 해줍니다. 이는 파일 처리 시 오류를 피하고 파일 시스템 조작의 신뢰성을 보장하는 데 중요합니다.

## 방법:
루비의 표준 라이브러리는 디렉토리의 존재 여부를 확인하기 위한 직관적인 메소드를 제공합니다. 여기 제3자 라이브러리가 필요 없는 순수 루비를 사용한 방법이 있습니다:

```ruby
require 'fileutils'

# 디렉토리가 존재하는지 확인
if Dir.exist?('/path/to/directory')
  puts '디렉토리가 존재합니다.'
else
  puts '디렉토리가 존재하지 않습니다.'
end
```
샘플 출력:
```
디렉토리가 존재합니다.
```
또는:
```
디렉토리가 존재하지 않습니다.
```

`Dir.exist?`를 사용하는 것 외에도 주어진 경로가 디렉토리인 경우 `true`를 반환하는 `File.directory?` 메소드를 활용할 수도 있습니다:

```ruby
if File.directory?('/path/to/directory')
  puts '디렉토리가 존재합니다.'
else
  puts '디렉토리가 존재하지 않습니다.'
end
```
`Dir.exist?` 및 `File.directory?`는 루비의 표준 라이브러리의 일부이며 외부 젬을 사용하지 않아도 되므로 디렉토리 확인을 위한 편리하고 효율적인 옵션입니다.
