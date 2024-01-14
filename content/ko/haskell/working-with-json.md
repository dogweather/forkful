---
title:                "Haskell: JSON으로 작업하기"
simple_title:         "JSON으로 작업하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

JSON은 대부분의 현대 프로그래밍 언어에서 자주 사용되고 있는 데이터 형식입니다. 따라서 Haskell에서도 JSON과 함께 작업하는 것이 중요한 기술입니다. 또한, JSON은 많은 기능을 지원하기 때문에 다양한 용도로 사용될 수 있습니다.

## 어떻게

Haskell에서 JSON을 다루는 방법은 매우 간단합니다. 먼저 `Data.Aeson` 모듈을 임포트해야 합니다. 그 후에, `FromJSON`과 `ToJSON` 타입 클래스를 사용하여 JSON 데이터와 Haskell 데이터를 서로 변환할 수 있습니다.

예를 들어, 다음과 같은 JSON 데이터가 있다고 가정해봅시다.

```Haskell
{"name": "John", "age": 25, "hobbies": ["reading", "gaming"]}
```

우리는 이를 다음과 같이 Haskell 데이터로 변환할 수 있습니다.

```Haskell
data Person = Person
    { name :: String
    , age :: Int
    , hobbies :: [String]
    } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

main :: IO ()
main = do
    let json = "{\"name\": \"John\", \"age\": 25, \"hobbies\": [\"reading\", \"gaming\"]}"
    let person = decode json :: Maybe Person
    print person
```

위 코드는 `Person`이라는 데이터 타입을 정의하고, 이를 `FromJSON`과 `ToJSON` 타입 클래스의 인스턴스로 만든 후, `decode` 함수를 사용하여 JSON 데이터를 `Person` 타입으로 변환합니다. 마지막으로 출력하여 확인할 수 있습니다.

```Haskell
Just Person {name = "John", age = 25, hobbies = ["reading", "gaming"]}
```

이제 `Person` 타입의 데이터를 JSON으로 다시 변환하는 예시를 살펴보겠습니다.

```Haskell
main :: IO ()
main = do
    let person = Person {name = "John", age = 25, hobbies = ["reading", "gaming"]}
    let json = encode person
    putStrLn json
```

위 코드는 `Person` 타입의 데이터를 `encode` 함수를 사용하여 JSON으로 변환하고, `putStrLn` 함수로 출력합니다.

```Haskell
{"name":"John","age":25,"hobbies":["reading","gaming"]}
```

## 자세히 살펴보기

더 깊이 들어가서 JSON을 처리하는 다른 방식에 대해 알아보겠습니다. `Data.Aeson` 모듈에서 제공하는 다양한 함수 및 타입을 사용하여 JSON 데이터를 더욱 유연하게 다룰 수 있습니다.

예를 들어, `object` 함수를 사용하면 키와 값의 쌍으로 이루어진 객체 형태의 JSON 데이터를 생성할 수 있습니다.

```Haskell
object [("name", "John"), ("age", Number 25), ("hobbies", Array [String "reading", String "gaming"])]
```

또한, `(.:)` 함수를 사용하여 `FromJSON` 타입 클래스의 인스턴스를 만들 때, 키 이름과 해당 값을 받아서 특정 타입으로 변환하는 함수를 작성할 수 있습니다.

```Haskell
instance FromJSON Person where
    parseJSON (Object v) = Person
        <$> v .: "name"
        <*> v .: "age"
        <*> v .: "hobbies"
```

이제 `Person` 타입으로 파싱할 때, 키 이름에 해당하는 값을 자동으로 `name`, `age`, `hobbies` 필드에 넣게 됩니다.

## 자세히 살펴보기

더 자세한 정보를 원한다면 아래 링크들을 참고해보세요!

## 관련 자료

- [Haskell에서 JSON 처리하기 (Haskell Wiki)](http://wiki.haskell.org/JSON)
- [