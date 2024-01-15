---
title:                "디렉토리 존재 여부 확인하기"
html_title:           "Ruby: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 왜

파일 또는 디렉토리가 존재하는지 확인하는 것은 프로그래밍에서 매우 유용합니다. 파일 미리 확인은 에러를 방지하고, 코드의 안정성을 높이며, 원하는 파일을 찾는 것을 보장합니다.

## 어떻게

```Ruby
if File.directory?("/Users/username/Documents")
  puts "디렉토리가 존재합니다."
else
  puts "디렉토리가 존재하지 않습니다."
end
```

위의 코드는 해당 디렉토리가 존재하는지 확인하는 간단한 예제입니다. 디렉토리 경로를 수정하고 실행하면 디렉토리가 존재하는지 여부가 출력됩니다.

## 깊이 파헤치기

디렉토리 존재 여부를 확인하는 방법에는 몇 가지 다른 방법이 있지만, 대부분의 경우 `File.directory?` 메소드가 가장 효율적입니다. 이 메소드는 해당 경로가 디렉토리인지를 불리언 값으로 반환합니다. `File.exist?` 메소드는 파일 뿐만 아니라 디렉토리 또한 존재하는지 확인합니다. 

## 또 다른 링크 더 보기

* Ruby 공식 문서: https://ruby-doc.org/core-3.0.0/File.html#method-c-directory-3F
* 파일 및 디렉토리 다루기 강좌: https://www.rubyguides.com/2015/05/working-with-files-ruby/