---
title:    "Ruby: 패턴에 일치하는 문자 삭제하기"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜
때로는 특정 패턴과 일치하는 문자를 삭제해야 할 필요가 있습니다. 이럴 때, 루비 프로그래밍을 사용하여 간단하게 해결할 수 있습니다.

## 방법
먼저, 삭제할 문자를 포함한 문자열을 변수에 할당합니다. 그리고 `gsub!` 메소드를 사용하여 해당 패턴과 일치하는 문자를 빈 문자열로 바꿉니다. 
예를 들어, 특정 단어를 모두 삭제하고 싶다면 다음과 같이 코드를 작성할 수 있습니다.

```Ruby
string = "안녕하세요! 이제는 루비 프로그래밍을 배우는 시간입니다."
string.gsub!(/루비/, "")
puts string 
```
출력 결과: 안녕하세요! 이제는 프로그래밍을 배우는 시간입니다.

또 다른 예로, 특정 문자를 모두 삭제하고 싶을 때는 다음과 같이 코드를 작성할 수 있습니다.

```Ruby
string = "루비 프로그래밍은 즐겁고 재미있습니까?"
string.gsub!(/[?]/, "") 
puts string 
```
출력 결과: 루비 프로그래밍은 즐겁고 재미있다

## 딥 다이브
위의 예시에서는 패턴을 정규식을 사용하여 지정하였지만, `gsub!` 메소드를 사용하는 방법은 다른 방법들도 있습니다. 예를 들어, `delete` 메소드를 사용하면 특정 문자를 삭제할 수 있습니다.

```Ruby
string = "딩동 댕동! 딩댕!"
string.delete!("동") 
puts string 
```
출력 결과: 딩 댕! 댕!

여러분이 원하는 방법에 따라 적절한 메소드를 선택해보세요. 또한 `gsub!`와 `delete!` 메소드 외에도 다양한 문자 삭제 방법들이 있으니, 더 알아보고 응용해보세요.

## 참고 자료
- [루비 공식 문서 - String 클래스](https://ruby-doc.org/core-2.7.0/String.html)
- [루비 API 문서 - gsub! 메소드](https://rubyapi.org/2.7/o/ruby#method-i-gsub-21)
- [루비 API 문서 - delete! 메소드](https://rubyapi.org/2.7/o/ruby-2.7#method-i-delete-21)