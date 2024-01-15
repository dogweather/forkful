---
title:                "패턴과 일치하는 문자 삭제"
html_title:           "Ruby: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜?

캐릭터 매칭 패턴을 삭제하는 작업에 참여하는 이유는 데이터나 문자열을 정제하거나 필요 없는 정보를 제거하기 위해서입니다.

## 어떻게?

우선, `gsub` 메소드를 사용하여 문자열에서 매칭되는 패턴을 찾아 삭제할 수 있습니다. 예를 들어, 다음과 같이 정규식을 사용하여 숫자를 모두 삭제할 수 있습니다:

```Ruby
target = "Apple123"
target.gsub!(/\d/, "")
```

위의 코드에서 `gsub!` 메소드는 `target` 문자열에서 숫자에 해당하는 정규식 패턴인 `\d`을 찾아 없애고, 그 결과인 `"Apple"`을 다시 `target` 변수에 할당합니다. 따라서 `target`의 값은 `"Apple"`이 됩니다.

더욱 복잡한 패턴을 삭제하고 싶다면, `gsub` 메소드의 두 번째 인자를 활용할 수 있습니다. 이 인자에는 삭제한 패턴에 대신 삽입할 문자열을 지정할 수 있습니다. 예를 들어, 알파벳과 숫자가 섞인 문자열에 대해 알파벳만 남기고 싶다면 다음과 같이 할 수 있습니다:

```Ruby
target = "Apple123"
target.gsub!(/[^a-zA-Z]/, "")
```

위의 코드에서 `gsub!` 메소드의 첫 번째 인자로는 `[^a-zA-Z]`이라는 정규식 패턴을 사용했습니다. 이 패턴은 알파벳을 제외한 모든 문자를 매칭하게 됩니다. 따라서 `target`의 값은 `"Apple"`이 됩니다.

## 깊이 파고들기

위에서 사용한 `gsub` 메소드는 문자열에서 오직 첫 번째 매칭된 패턴만을 삭제합니다. 만약 모든 매칭되는 패턴을 삭제하고 싶다면 어떻게 해야 할까요? 이 경우에는 `gsub!` 메소드 대신 `gsub` 메소드를 사용하면 됩니다. `gsub` 메소드는 `gsub!` 메소드와 다르게, 문자열에서 매칭되는 모든 패턴을 삭제합니다.

또, `gsub` 메소드는 문자열 뿐만 아니라 `Hash`나 `Array` 객체에서도 사용할 수 있습니다. 이때는 첫 번째 인자로 삭제할 값, 두 번째 인자로 그 값을 대체할 값(문자열 또는 정규식)을 넣으면 됩니다. 예를 들어, 다음과 같이 해시에서 매칭되는 값을 모두 삭제할 수 있습니다:

```Ruby
target = { a: 1, b: 2, c: 3 }
target.gsub!(3, "")
```

위의 코드에서 `gsub!` 메소드는 `target` 해시에서 값이 `3`인 `key-value` 쌍을 모두 제거합니다. 따라서 `target`에는 `{ a: 1, b: 2 }`만 남게 됩니다.

## 더 알아보기

Ruby의 `gsub` 메소드를 사용하여 문자열 또는 다른 객체에서 매칭되는 패턴을 삭제하는 방법에 대해 알아봤습니다. 하지만 이 외에도 Ruby에서는 다양한 패턴 매칭 및 삭제 방법을 제공합니다. 자세한 내용은 아래의 링크를 참고해 보시기 바랍니다.

## 더 알아보기
- [Ruby 문서 - **gsub** 메소드](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Ruby 문서 - 정규식(Regular Expressions)](https://ruby-doc.org/core-2.7.1/Regexp.html)