function createProfiles() {
  const profiles = [
    {
      name: "Angela Onwuachi-Willig",
      year: 1994,
      blurb:
        "Angela is dean of the Boston University School of Law. Angela majored in American studies with a concentration in African American studies at Grinnell.",
    }
    // Add the rest of the alumni information in a similar format
  ];

  let profileContainer = document.getElementById("profile-container");

  profiles.forEach((profile) => {
    let card = document.createElement("div");
    card.classList.add("card");

    let cardBody = document.createElement("div");
    cardBody.classList.add("card-body");

    let name = document.createElement("h5");
    name.classList.add("card-title");
    name.textContent = profile.name;

    let year = document.createElement("h6");
    year.classList.add("card-subtitle", "mb-2", "text-muted");
    year.textContent = `Year: ${profile.year}`;

    let blurb = document.createElement("p");
    blurb.classList.add("card-text");
    blurb.textContent = profile.blurb;

    cardBody.appendChild(name);
    cardBody.appendChild(year);
    cardBody.appendChild(blurb);
    card.appendChild(cardBody);
    profileContainer.appendChild(card);
  });
}
