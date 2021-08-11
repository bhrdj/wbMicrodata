# these two functions, iso_to_emoji_unicode and iso_to_emoji_ascii, convert two-letter iso country codes
# (eg. australia = AU) to the unicode control sequences used for their flag emoji. the _unicode function
# outputs the actual unicode characters (printed in R in ascii but stored in ascii); the _ascii function
# outputs an ascii representation (with the argument ligature_sep in between the two strings).
import::here(stringr)

# iso_to_emoji_unicode: for a vector of two-letter iso codes, returns a
# corresponding vector of two-character unicode control sequences.
# (nb: R prints them in ascii, but they're really stored as unicode characters)
# great for use with countrycode and emojifont!
iso_to_emoji_unicode = function(iso_codes)
{
  # check input
  if (!any(
    nchar(iso_codes) == 2 |
    is.na(iso_codes)))
  {
    stop('iso_to_emoji: ISO codes must be two (2) letters long.')
  }
  if (!any(
    str_detect(iso_codes, pattern = '[a-zA-Z][a-zA-Z]') |
    is.na(iso_codes)))
  {
    stop('iso_to_emoji: ISO codes must be letters only.')
  }
  
  # substitute unicode regional indicator symbols for the original characters
  return(str_replace_all(str_to_lower(iso_codes), c('a' = '🇦', 'b' = '🇧', 'c' = '🇨',
    'd' = '🇩', 'e' = '🇪', 'f' = '🇫', 'g' = '🇬', 'h' = '🇭', 'i' = '🇮',
    'j' = '🇯', 'k' = '🇰', 'l' = '🇱', 'm' = '🇲', 'n' = '🇳', 'o' = '🇴',
    'p' = '🇵', 'q' = '🇶', 'r' = '🇷', 's' = '🇸', 't' = '🇹', 'u' = '🇺',
    'v' = '🇻', 'w' = '🇼', 'x' = '🇽', 'y' = '🇾', 'z' = '🇿')))
}

# iso_to_emoji_ascii: for a vector of two-letter iso codes, returns
# a corresponding vector of ascii-formatted unicode control sequences.
# great for downloading files named with unicode control points!
iso_to_emoji_ascii = function(iso_codes, ligature_sep = '-')
{
  # check input
  if (!any(
    nchar(iso_codes) == 2 |
    is.na(iso_codes)))
  {
    stop('iso_to_emoji: ISO codes must be two (2) letters long.')
  }
  if (!any(
    str_detect(iso_codes, pattern = '[a-zA-Z][a-zA-Z]') |
    is.na(iso_codes)))
  {
    stop('iso_to_emoji: ISO codes must be letters only.')
  }
  
  # add each letter's position in the alphabet to the start of the
  # regional indicator symbol block; format as string
  iso_codes %<>% str_to_lower
  return(
    paste0(
      as.hexmode(0x1f1e5 + match(substr(iso_codes, 1, 1), letters)),
      ligature_sep,
      as.hexmode(0x1f1e5 + match(substr(iso_codes, 2, 2), letters))))
}