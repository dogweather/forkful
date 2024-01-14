---
title:                "Haskell: 「csvとの作業」"
simple_title:         "「csvとの作業」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

**Why** 

CSVファイルはよく使われるデータフォーマットであり、Haskellで簡単に扱うことができます。Haskellの強力な型システムと多様なライブラリのおかげで、CSVファイルを処理することはとても便利です。 

**How To** 

まず、HaskellでCSVファイルを扱うためにData.Csvモジュールをインポートします。次に、パースするデータ型とCSVファイルのフォーマットを定義します。例えば、以下のようになります。 

```Haskell 
import Data.Csv 

data Person = Person 
 { name :: !String 
 , age :: !Int 
 , city :: !String 
 } 

instance FromNamedRecord Person where 
 parseNamedRecord m = Person 
  <$> m .: "Name" 
  <*> m .: "Age" 
  <*> m .: "City" 

personCsv :: ByteString 
personCsv = "Name,Age,City\nJohn,24,New York\nJane,30,Tokyo" :: ByteString 
``` 

この例では、Personというデータ型を定義し、FromNamedRecordクラスのインスタンスにすることでパースすることができるようになります。また、ファイルのフォーマットをpersonCsvというバイト文字列で定義しています。 

次に、パースしたデータを取得するために、parseCsvメソッドを使用します。 

```Haskell 
csvData :: Either String (Vector Person) 
csvData = parseCsv personCsv
``` 

このようにすることで、データを右辺のPersonのベクターとして取得することができます。また、エラーが発生した場合はエラーメッセージが返されます。 

**Deep Dive** 

Haskellのデータ型の強力な機能を活用することで、さまざまなデータフォーマットを扱うことができます。例えば、以下のように定義することで、タイプセーフなCSVパーサーを作ることができます。 

```Haskell 
import Data.Aeson 
import Data.Csv 
import Data.Time.Calendar 

data Movie = Movie 
 { title :: String 
 , releaseDate :: Day 
 } 

instance FromNamedRecord Movie where 
 parseNamedRecord m = 
  Movie <$> m .: "Title" <*> m .: "Release Date" 

instance ToNamedRecord Movie where 
 toNamedRecord (Movie title releaseDate) = 
  namedRecord [ "Title" .= title, "Release Date" .= releaseDate ] 

instance ToJSON Movie where 
 toJSON (Movie title releaseDate) = 
  object [ "Title" .= title, "Release Date" .= releaseDate ] 

instance FromJSON Movie where 
 parseJSON (Object v) = 
  Movie <$> v .: "Title" <*> v .: "Release Date" 

movieCsv :: ByteString 
movieCsv = "Title,Release Date\nInterstellar,2014-11-07\nInception,2010-07-16" 

movies :: Either String (Vector Movie) 
movies = parseCsv movieCsv 

movieJson :: Either String [Movie] 
movieJson = eitherDecode movieCsv 
``` 

上の例では、Movieというデータ型を定義し、FromNamedRecordクラスとToNamedRecordクラスに定義されているメソッドを使用してCSVファイルのパースと作成を行います。また、ToJSONクラスとFromJSONクラスを使用してJSON形式のデータを取得することもできます。 

**See Also** 

- [HaskellでのCSVファイルの扱い方](https://qiita.com/h-yoshikawa/items/5c572232440ecc1d3d87) 
- [HaskellのData.Csvモジュールのドキュメント](https://hackage.haskell.org/package/cassava-0.5.0.0/docs/Data-C