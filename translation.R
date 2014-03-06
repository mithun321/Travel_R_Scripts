library(translate)
set.key('GOOGLE-KEY')


query = "how are you?"

print(query)

txt = translate(query, "en", "de", key = "GOOGLE_KEY")

set.key('http://translate.google.com/')
translate('Hello, world!', 'en', 'de')