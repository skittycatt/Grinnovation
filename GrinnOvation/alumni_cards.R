generateAlumniCards <- function() {
  alumni_list <- list(
    list(
      name = "Yesenia Ayala",
      year = 2018,
      subtitle = "IES Abroad's 2017 Global Citizen of the Year",
      blurb = "She was honored by the White House in 2015. She was a Spanish and sociology double major with a Latin American studies concentration."
    ),
    list(
      name = "Herbie Hancock",
      year = 1960,
      subtitle = "Pianist, Composer, Bandleader, and Recording Artist",
      blurb = "Herbie double-majored in music and electrical engineering. He has won the Grammy for Album of the Year in 2008 for River: The Joni Letters."
    ),
    list(
      name = "Joshua Ekirikubinza-Tibatemwa",
      year = 2019,
      subtitle = "Grinnell's First Modern-Era Olympian",
      blurb = "He swam the 50-meter freestyle during the 2016 Rio Olympics on behalf of Uganda. Joshua majored in computer science and economics."
    ),
    list(
      name = "Angela Onwuachi-Willig",
      year = 1994,
      subtitle = "Dean of the Boston University School of Law",
      blurb = "Angela majored in American studies with a concentration in African American studies at Grinnell."
    ),
    list(
      name = "Christine Thornburn",
      year = 1992,
      subtitle = "Retired American Professional Road Cyclist",
      blurb = "She represented the United States in the 2004 and 2008 Olympic Games. She ran cross country and majored in chemistry at Grinnell."
    ),
    list(
      name = "Margaret Tandoh",
      year = 1993,
      subtitle = "Time Magazine 'Person of the Year' in 2014",
      blurb = "She volunteered to return to her home country of Liberia to fight the Ebola epidemic. She majored in biology at Grinnell."
    ),
    list(
      name = "Kumail Nanjiani",
      year = 2001,
      subtitle = "Actor, Comedian, and Screenwriter",
      blurb = "He was nominated for an Academy Award for Best Original Screenplay for The Big Stick. He majored in computer science and philosophy at Grinnell."
    ),
    list(
      name = "Cara Stein",
      year = 1984,
      subtitle = "Chief Talent Officer and Senior Vice President of NBCUniversal",
      blurb = "She served on the board of directors of the William Morris Agency and the New York Television Festival. She was a political science major at Grinnell."
    ),
    list(
      name = "Tom Cech",
      year = 1970,
      subtitle = "1989 Nobel Prize Winner in Chemistry",
      blurb = "He discovered the catalytic properties of RNA and heads the BioFrontiers Institute at the University of Colorado, Boulder. He majored in chemistry at Grinnell."
    ),
    list(
      name = "Sarah Labowitz",
      year = 2004,
      subtitle = "Assistant Director for the City of Houston Housing and Community Development Department",
      blurb = "She served under Hillary Clinton at the U.S. Department of State."
    ),
    list(
      name = "David White",
      year = 1990,
      subtitle = "National Executive Director of SAG-AFTRA",
      blurb = "He majored in political science at Grinnell College."
    ),
    list(
      name = "Robert Noyce",
      year = 1949,
      subtitle = "Co-Inventor of the Integrated Circuit and Co-Founder of Intel",
      blurb = "He held 16 patents and received many awards. He majored in physics at Grinnell."
    )
  )
  
  cards <- lapply(alumni_list, function(alumnus) {
      div(
        class = "grid-item",
          h5(class = "card-title", paste(alumnus$name, "(", alumnus$year, ")")),
          h6(class = "card-subtitle mb-2 text-muted", alumnus$subtitle),
          p(class = "card-text", alumnus$blurb)
      )
    
  })
  
  return(cards)
}