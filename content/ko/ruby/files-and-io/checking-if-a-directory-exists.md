---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:23.528480-07:00
description: "\uBC29\uBC95: \uB8E8\uBE44\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB294 \uB514\uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC \uC5EC\uBD80\uB97C \uD655\
  \uC778\uD558\uAE30 \uC704\uD55C \uC9C1\uAD00\uC801\uC778 \uBA54\uC18C\uB4DC\uB97C\
  \ \uC81C\uACF5\uD569\uB2C8\uB2E4. \uC5EC\uAE30 \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uAC00 \uD544\uC694 \uC5C6\uB294 \uC21C\uC218 \uB8E8\uBE44\uB97C \uC0AC\uC6A9\
  \uD55C \uBC29\uBC95\uC774 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:56.017017-06:00'
model: gpt-4-0125-preview
summary: "\uB8E8\uBE44\uC758 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB294 \uB514\
  \uB809\uD1A0\uB9AC\uC758 \uC874\uC7AC \uC5EC\uBD80\uB97C \uD655\uC778\uD558\uAE30\
  \ \uC704\uD55C \uC9C1\uAD00\uC801\uC778 \uBA54\uC18C\uB4DC\uB97C \uC81C\uACF5\uD569\
  \uB2C8\uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

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
