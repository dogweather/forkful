---
title:    "Ruby: 디렉토리가 존재하는지 확인하기"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

이 글에서는 Ruby로 디렉토리가 존재하는지 확인하는 방법을 알려드립니다. 디렉토리의 존재 여부를 확인함으로써 파일 및 폴더를 관리하는 프로그래밍 작업을 더욱 효율적으로 수행할 수 있기 때문에 디렉토리 확인은 중요한 개념입니다.

## 사용 방법

우선, Ruby에서는 디렉토리의 존재 여부를 확인하는 `Dir.exist?()` 메소드를 제공합니다. 이 메소드는 디렉토리가 존재할 경우 `true`를, 존재하지 않을 경우 `false`를 반환합니다. 예시 코드를 살펴보겠습니다.

```Ruby
# 디렉토리가 존재하는 경우
puts Dir.exist?("documents")
# => true

# 디렉토리가 존재하지 않는 경우
puts Dir.exist?("images")
# => false
```

또 다른 방법으로는 `FileTest.directory?()` 메소드를 사용하는 것입니다. 이 방법은 `Dir.exist?()`와 유사하지만 조금 다른 차이점이 있습니다. `FileTest.directory?()`는 디렉토리가 아니라 파일일 경우에도 `true`를 반환합니다. 예시 코드를 살펴보겠습니다.

```Ruby
# 디렉토리가 존재하는 경우
puts FileTest.directory?("documents")
# => true

# 디렉토리가 아닌 파일인 경우
puts FileTest.directory?("index.html")
# => true
```

위의 코드에서 `Dir.exist?()`는 `documents` 디렉토리가 존재하기 때문에 `true`를 반환하지만 `FileTest.directory?()`는 `index.html`이라는 파일 이름이 `directory`로 끝나기 때문에 디렉토리로 간주하고 `true`를 반환합니다.

## 깊은 이해

Ruby에서 디렉토리 확인을 수행하려면 파일 또는 디렉토리의 경로를 정확히 지정해야 합니다. 경로는 디렉토리를 나타내는 문자열입니다. 따라서 존재하지 않는 경로를 입력하는 경우에는 항상 `false`를 반환합니다.

또한, 디렉토리를 확인하는 작업은 해당 디렉토리가 포함하는 파일과 폴더의 존재 여부를 확인하는데 유용하게 사용될 수 있습니다. 예를 들어, 파일 및 폴더의 리스트를 가져오는 작업을 수행하기 전에 디렉토리가 존재하는지 확인하는 것이 좋습니다.

## 연관 자료

- [Ruby 공식 문서 - 디렉토리 존재 여부 확인](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F)
- [Ruby 공식 문서 - 파일 및 디렉토리 경로](https://ruby-doc.org/core-2.7.1/File.html)
- [위키북스 - 따라 하며 배우는 루비](https://wikibook.co.kr/ruby-for-adult/)
- [루비로 배우는 객체지향 디자인](https://www.aladin.co.kr/shop/wproduct.aspx?ItemId=104969237)
- [Ruby를 이용한 웹 애플리케이션 개발](https://www.aladin.co.kr/shop/wproduct.aspx?ItemId=57027643) 

## 참고 자료

- [누구나 쉽게 배우는 Ruby (생활코딩)](https://opentutorials.org/course/2057)
- [루비