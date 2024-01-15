---
title:                "부분 문자열 추출"
html_title:           "Haskell: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜
문자열에서 부분 문자열을 추출하는 것이 중요한 이유는 데이터 처리 및 텍스트 분석과 같은 작업에서 유용하기 때문입니다. 예를 들어, 사용자가 입력한 검색어를 처리하거나, 웹 페이지에서 특정 내용을 추출하는 데 필요한 경우 등에 사용될 수 있습니다.

## 어떻게
부분 문자열을 추출하는 방법은 `Data.List` 라이브러리의 `isPrefixOf`와 `isSuffixOf` 함수를 활용하면 간단하게 구현할 수 있습니다. 또한 `take`와 `drop` 함수를 사용하여 원하는 범위의 부분 문자열을 추출할 수도 있습니다.

```Haskell
-- isPrefixOf 함수를 사용한 부분 문자열 추출 예시
import Data.List (isPrefixOf, isSuffixOf)

main = do
  let email = "example@gmail.com"
  if "example" `isPrefixOf` email
    then putStrLn ("이메일 주소는 example로 시작합니다.")
    else putStrLn ("이메일 주소는 example로 시작하지 않습니다.")
  if ".com" `isSuffixOf` email
    then putStrLn ("이메일 주소는 .com으로 끝납니다.")
    else putStrLn ("이메일 주소는 .com으로 끝나지 않습니다.")

-- take와 drop 함수를 사용한 부분 문자열 추출 예시
import Data.List (isPrefixOf, isSuffixOf)

main = do
  let sentence = "우울한 날에는 산책을 하거나 친구와 얘기하는 것이 좋아요."
  putStrLn (take 11 sentence) -- "우울한 날에는"
  putStrLn (drop 12 sentence) -- "산책을 하거나 친구와 얘기하는 것이 좋아요."
```

## 딥 다이브
부분 문자열을 추출하기 위해 사용되는 `isPrefixOf`, `isSuffixOf`, `take`, `drop` 함수들은 속도가 빠르며 효율적인 알고리즘을 가지고 있어 대부분의 경우에 적합합니다. 또한, `Data.Text` 라이브러리를 이용하여 문자열을 처리하면 더욱 효율적인 부분 문자열 추출이 가능합니다.

## 관련 자료
- [Haskell 공식 웹사이트](https://www.haskell.org/)
- [Haskell 시작하기: 모나드에서 모나드로](https://tech.kakao.com/2019/08/01/monoid-monad/)
- [Haskell로 프로그래밍하기 - 우주를 구하라!](https://academy.realm.io/kr/posts/tech/functional-programming-in-haskell/)
- [Data.List 문서](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html)
- [Data.Text 문서](https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text.html)